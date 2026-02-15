Export is on Grid G1
Import is on Grid G2

Connection is attempted:
  1. can connect is "yes"
  2. requires extension is "yes"
  3. add RegridExtension(G1,G2) to component
  
  
Problems:
  - "component" is not available at the point wher connection is
    processed.  We are deep inside registry which is owned by
    component.  Backward pointer would be "bad".
    
Option:
  - Have registry track extensions?  Then GC could access to then determine actions that are to be generated.
  - Have GC initialize phase invoke a `add_to_component` on registry that does the rest.
  
