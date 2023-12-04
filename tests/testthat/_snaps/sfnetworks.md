# sfnetworks sfnetwork number Bad

    Code
      sfnetworks_sfnetwork()(10)
    Condition
      Error:
      ! type mismatch
      `typeof(value)`: "double"    
      `expected`:      "sfnetworks"

# sfnetworks sfnetwork string Bad

    Code
      sfnetworks_sfnetwork()("a")
    Condition
      Error:
      ! type mismatch
      `typeof(value)`: "character" 
      `expected`:      "sfnetworks"

# sfnetworks sfnetwork null_ok Bad

    Code
      sfnetworks_sfnetwork(null_ok = FALSE)(NULL)
    Condition
      Error:
      ! type mismatch
      `typeof(value)`: "NULL"      
      `expected`:      "sfnetworks"

