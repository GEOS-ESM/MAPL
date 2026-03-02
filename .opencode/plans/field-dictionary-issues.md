# Field Dictionary Integration - GitHub Issues

**Created:** 2026-02-22  
**Status:** Planning Phase

## Quick Links

### MAPL Repository
- **Epic:** [#4438 - Integrate Extended Field Dictionary into MAPL3](https://github.com/GEOS-ESM/MAPL/issues/4438)
- **Phase 0:** [#4439 - Schema Refinement & Team Review](https://github.com/GEOS-ESM/MAPL/issues/4439) ⚠️ **START HERE**
- **Phase 1:** [#4440 - Core Infrastructure](https://github.com/GEOS-ESM/MAPL/issues/4440)
- **Phase 2:** [#4441 - Integration & Validation](https://github.com/GEOS-ESM/MAPL/issues/4441)
- **Phase 3:** [#4442 - Build & Installation](https://github.com/GEOS-ESM/MAPL/issues/4442)
- **Phase 5:** [#4443 - ACG Enhancement](https://github.com/GEOS-ESM/MAPL/issues/4443)
- **Phase 6:** [#4444 - Documentation & Training](https://github.com/GEOS-ESM/MAPL/issues/4444)

### GEOS_FieldDictionary Repository
- **Phase 1:** [#1 - Update Schema Documentation](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/1)
- **Phase 4:** [#2 - Migrate 1,500 Entries](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/2)
- **Workflow:** [#3 - Field Verification Workflow](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/3)

## Full Plan
See [field-dictionary-integration.md](./field-dictionary-integration.md) for complete technical plan.

## Timeline Summary

```
Phase 0: Schema Review       [1 week]   ← START HERE
  └─ Team consensus required before proceeding
  
Phase 1: Core Infrastructure [2 weeks]
  ├─ MAPL: Fortran types, parser
  └─ GEOS_FieldDictionary: Schema docs
  
Phase 2: Integration         [2 weeks]
  └─ VariableSpec integration
  
Phase 3: Build & Install     [1 week]
  └─ CMake, mepo integration
  
Phase 4: Dictionary Migration [2-3 weeks] (parallel with Phase 5-6)
  └─ GEOS_FieldDictionary: Migrate 1,500 entries
  
Phase 5: ACG Enhancement     [1 week]
  └─ ACG dictionary integration
  
Phase 6: Documentation       [1 week]
  └─ User guides, training
```

**Total Estimated Time:** 8-10 weeks (some phases can overlap)

## Current Status

- ✅ Plan created and saved to `.opencode/plans/`
- ✅ All issues created with cross-references
- ⏳ **Next:** Schedule Phase 0 schema review meeting
- ⏳ Waiting for team consensus on schema design

## Key Decisions Pending (Phase 0)

1. Field naming: `units` vs `canonical_units`?
2. Field naming: `short_names` vs `aliases`?
3. Keep `incomplete` flag or replace with `verification_status`?
4. Physical dimension vocabulary: controlled vs free-form?
5. Migration defaults for existing 1,500 entries

## Success Criteria

- [ ] Schema approved by team (Phase 0)
- [ ] Core infrastructure implemented and tested (Phase 1)
- [ ] Integration working with overrides (Phase 2)
- [ ] Dictionary installed and accessible (Phase 3)
- [ ] All 1,500 entries migrated (Phase 4)
- [ ] ACG uses dictionary (Phase 5)
- [ ] Documentation complete (Phase 6)

---

**See also:**
- Complete plan: [field-dictionary-integration.md](./field-dictionary-integration.md)
- MAPL Epic: https://github.com/GEOS-ESM/MAPL/issues/4438
