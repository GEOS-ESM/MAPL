# Configurable callback State (mark 1)

**Audience:** developers writing unit tests for code that calls
`ESMF_MethodExecute` on a State.
**Describes:** `mapl_ConfigurableCallback_mod` and
`mapl_ConfigurableCallbackSpec_mod` in `superstructure/state/`.

A configurable callback State is an `ESMF_State` whose methods are attached
by MAPL, configured in YAML. It stands in for the gridcomp that would
normally attach the methods, so client code can be tested alone. When the
client executes one of its methods, the State:

1. **records** what the client set before the call (attributes, field
   copies), the call count, and the call order;
2. **fails on demand** with a configured `userRc`;
3. **supplies** attributes and field values the client reads after the call.

The client calls `ESMF_MethodExecute` (and the legacy
`ESMF_AttributeSet/Get`) as usual; it does not know the State is a mock.
There is no interface registry, no contract checking, and no gridcomp form.

## Test-support API

```fortran
use mapl_ConfigurableCallback_mod

call ConfigurableCallbackCreate(state, hconfig, grid=grid, num_levels=nlev, name='aci', rc=rc)
n = ConfigurableCallbackCallCount(state, label, rc=rc)
call ConfigurableCallbackCalls(state, labels, rc=rc)                     ! all methods, call order
call ConfigurableCallbackAttrSeen(state, label, name, values, rc=rc)     ! one string per call
call ConfigurableCallbackFieldSeen(state, label, name, call_no, field, rc=rc)
```

- `grid` is required if `fields` is configured; `num_levels` is required if
  any field has `dims: xyz`. The YAML key `geometry` is rejected.
- `ConfigurableCallbackAttrSeen` returns values as strings; an attribute that
  was absent at a call is recorded as an empty string. It fails if the method
  does not record that attribute.
- `ConfigurableCallbackFieldSeen` returns the copy of the field taken at call
  `call_no`. Copies live in a nested State `mapl_mock_record`, named by
  `ConfigurableCallbackRecordName(label, name, call_no)`. That nested State
  is added only if some method has `record.fields`, so otherwise the State's
  items are exactly the configured fields.

## YAML

The HConfig passed to `ConfigurableCallbackCreate` describes one State:

```yaml
state_attributes:                   # set once at build time
  aerosol_modes: [du001, ss001]     # string, integer, real, logical, or a list
  air_temperature: T                # a field name, read by the client
fields:                             # created at build time on the given grid
  T:   {dims: xyz}                  # dims: xy or xyz; typekind: R4 (default) or R8
  num: {dims: xyz}
methods:                            # label -> behavior; order = slot order
  aerosol_activation_properties:
    record:
      attributes: [aerosol_mode]    # value seen at every call
      fields: [T]                   # copy of the field at every call
    fail_on_call: {3: 17}           # third call returns userRc = 17
    supply:
      attributes:
        aerosol_number_concentration: num
      fields:
        num:
          by_attribute:
            key: aerosol_mode
            du001: {constant: 1.0e8}
            ss001: {constant: 5.0e7}
          default: error
```

### Supply forms

| Form | Meaning |
|---|---|
| `constant: v` | same value everywhere (fields) |
| `profile: [v_1, ..., v_K]` | one value per level, uniform horizontally; `xyz` fields only, `K = num_levels` |
| `by_attribute: {key: a, <value>: <form>, ...}` plus `default:` | form chosen by the value of attribute `a` at the call; `default` is required, is either a form or `error`, and may sit beside `by_attribute` or inside it (not both) |
| `by_call: [<form>, ...]` | the n-th call uses the n-th form; a call past the end is an error |
| a literal (attributes only) | the attribute value, set at every call |

`function:` (per-cell expressions) is not supported.

### Order within one call

1. Increment the call count and append the label to the call order.
2. Record the configured attributes and fields.
3. If `fail_on_call` names this call, return its `rc` as `userRc` and supply
   nothing.
4. Supply attributes and fields.

Any error raised by the mock itself during a call (for example a
`by_attribute` lookup with an unlisted value and `default: error`, or a
`by_call` list shorter than the number of calls) returns `userRc =
MOCK_INTERNAL_ERROR_RC` (-999901, public from `mapl_ConfigurableCallback_mod`),
with a message naming the cause. That value is reserved: `fail_on_call`
rejects it at `ConfigurableCallbackCreate`, so a client test can always tell a
configured failure from a broken mock.

### Limits and errors

- At most `MAX_CALLBACK_SLOTS` (8) methods per State. ESMF does not pass the
  label to the attached routine, so each method is attached to one of eight
  fixed slot procedures; a ninth method fails at `ConfigurableCallbackCreate`.
- Configuration errors (unknown key, bad type, missing `default`, `profile`
  of the wrong length) fail at `ConfigurableCallbackCreate`, naming the key.
- The labels `calls`, `labels`, and `num_levels` are reserved (they are mock
  bookkeeping keys under `mapl_mock/`) and fail at `ConfigurableCallbackCreate`.
- Executing a label that is not configured behaves as ESMF defines it:
  `existflag` returns false, and without `existflag` `ESMF_MethodExecute`
  returns a non-zero `rc`.
- Names and values are limited to 256 characters (`MAX_NAME_LEN`): a
  recorded attribute value longer than that fails the call, and a longer
  YAML key (label, field or attribute name) is silently truncated.

## Where data lives

All mock bookkeeping is in the State's own `ESMF_Info`, under the reserved
namespace `mapl_mock/`, so several mock States can coexist with no shared
module state:

```
mapl_mock/labels                        configured labels, slot order
mapl_mock/num_levels                    num_levels passed to Create (0 if absent)
mapl_mock/calls                         labels in call order, all methods
mapl_mock/<label>/call_count            integer
mapl_mock/<label>/attr/<name>           values seen, one per call
mapl_mock/<label>/config                the method's configuration
mapl_mock/<label>/slot                  slot procedure index (1-8)
```

Client-visible attributes are set and read with the legacy
`ESMF_AttributeSet/Get` calls, which ESMF stores under the Info key
`ESMF/General/`. Do not put client data under `mapl_mock/`.

Tests: `superstructure/state/tests/Test_ConfigurableCallback.pf`
(ctest `MAPL.state.tests`), with configurations in
`superstructure/state/tests/configurable_callback_scenarios/`.
