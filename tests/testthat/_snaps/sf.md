# SF sfg Number Bad

    Code
      sf_sfg()(1)
    Condition
      Error:
      ! Not geometry
      `class(value)`: "numeric"
      `expected`:     "sfg"    

# SF sfg sfc Bad

    Code
      sf_sfg()(sf::st_sfc())
    Condition
      Error:
      ! Not geometry
      `class(value)`: "sfc_GEOMETRY" "sfc"
      `expected`:     "sfg"               

# SF sfg types Bad

    Code
      sf_sfg(types = c("LINESTRING", "POINT"))(sf::st_multilinestring())
    Condition
      Error:
      ! Wrong geometry type
      `sf::st_geometry_type(value)`: "MULTILINESTRING"        
      `expected`:                    "LINESTRING"      "POINT"

# SF sfc Number Bad

    Code
      sf_sfc()(1)
    Condition
      Error:
      ! Not geometry column
      `class(value)`: "numeric"
      `expected`:     "sfc"    

# SF sfc sfc Bad

    Code
      sf_sfc()(sf::st_point())
    Condition
      Error:
      ! Not geometry column
      `class(value)`: "XY"  "POINT" "sfg"
      `expected`:     "sfc"              

# SF sfc types Bad

    Code
      sf_sfc(types = c("LINESTRING", "POINT"))(sf::st_polygon() %.>% sf::st_sfc(.))
    Condition
      Error:
      ! Wrong geometry type
      `sf::st_geometry_type(value[[1]])`: "POLYGON"           
      `expected`:                         "LINESTRING" "POINT"

# SF sf base number Bad

    Code
      sf_sf()(10)
    Condition
      Error:
      ! Not sf object
      `class(value)`: "numeric"
      `expected`:     "sf"     

# SF sf base sfc Bad

    Code
      sf_sf()(sf::st_sfc())
    Condition
      Error:
      ! Not sf object
      `class(value)`: "sfc_GEOMETRY" "sfc"
      `expected`:     "sf"                

# SF sf bad column name Bad

    Code
      sf_sf()(nc2)
    Condition
      Error:
      ! Active geometry column does not exists
          attr(value, "sf_column") | expected                  
      [1] "geometry"               - "AREA"      [1]           
                                   - "PERIMETER" [2]           
                                   - "CNTY_"     [3]           
                                   - "CNTY_ID"   [4]           
                                   - "NAME"      [5]           
                                   - "FIPS"      [6]           
                                   - "FIPSNO"    [7]           
                                   - "CRESS_ID"  [8]           
                                   - "BIR74"     [9]           
                                   - "SID74"     [10]          
      ... ...                        ...         and 5 more ...

# SF sf df_opts Bad

    Code
      sf_sf(df_opts = new_df_opts)(sf_example)
    Condition
      Error:
      ! column AREA, type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# SF sf active_column Bad

    Code
      sf_sf(active_column = "geom")(sf_example)
    Condition
      Error:
      ! SF does not have the required active geometry column
      `attr(value, "sf_column")`: "geometry"
      `expected`:                 "geom"    

# SF sf active_types Bad

    Code
      sf_sf(active_types = "POLYGON")(sf_example)
    Condition
      Error:
      ! column geometry, Wrong geometry type
      `sf::st_geometry_type(value[[1]])`: "MULTIPOLYGON"
      `expected`:                         "POLYGON"     

# SF sf active_types multi Bad

    Code
      sf_sf(active_types = c("MULTIPOLYGON", "POLYGON"))(sf_example_multi)
    Condition
      Error:
      ! column geometry, Wrong geometry type
      `sf::st_geometry_type(value[[109]])`: "POINT"                 
      `expected`:                           "MULTIPOLYGON" "POLYGON"

# sf sfg invalid geometry

    Code
      sf_sfc(only_valid = TRUE)(data)
    Output
      [1] 1
    Condition
      Error in `f()`:
      ! This geometries are not valid.

