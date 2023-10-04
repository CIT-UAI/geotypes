# geotypes
Basic spatial types for static types on R


## Base types


### List


```R
geotypes::List(
 types = list(
   #hi key must be an integer
   "hi" = typed::Integer(),
   #The second element, must be not named and be a double
   typed::Double(),
   #the name element must be a character
   name = typed::Character()
 ),
 #every element not in types will have this check
 default_type = typed::Integer(),
 #If "all_of", the list must contain all the elements in the types param
 select = "any_of",
 #If TRUE, it can only exists and all of them the values and types declared on the types param
 only_vals = FALSE,
 empty_ok = TRUE,
 null_ok = TRUE,
 anyNA = FALSE
)
```


Lists are complex type of elements, because they are used without taking in consideration technical details, similar in how Lists handle to itself.


```list(a = 1, b = 2)``` is different from ```list(b = 2, a = 1)```


Lists have numbered elements, even if they are named, but ppl usually have no interest in the order of the named elements.


Due to this, this List follows some rules.


- All named elements will not check their position on the list
- All non-named elements will check their position, this is needed when you declare a set of elements to follow some rules.


```R
List(
 types = list(
   #1
   a = typed::Integer(),
   #2
   typed::Double,
   b = typed::Character()
 ),
 default_type = typed::Integer()
)
```


Lets eval the next lists:


```R
l1 <- list(a = 1:10, 3, b = "hi", 5:6, "ho")
```


This one will pass the tests, first, all of them will match the types here, the number ```3``` must be double, because is the second element on ```types``` and also a non-named one, the third element is named and match the type.


The final two elements are not in ```types``` so will use ```default_type``` as a check.


This list will also be valid.


```R
l1 <- list(b = "ho", 3, a = 1:10, 5:6, , "ho")
```


Follow the same rules, just remember it does not check if the named elements follow the order, only matters to exist and will check the types, but not the same for non-named elements.


```R
l1 <- list(a = 1:10, 5:6, b = "hi", 5:6, "ho")
```


Here will not pass, because the non-named elements must follow the position and the type, the second element on ```types``` is non-named and must be double.


### Data.frame




```R
geotypes::Data.frame(
 #Set assertions for every column
 columns = list(
   a = typed::Integer(),
   b = typed::Double(),
   c = typed::Character()
 ),
 #every column not in columns param will have this check
 default_type = typed::Integer(),
 #If "all_of", the df must contain all the columns in the columns param
 select = "any_of",
 #If TRUE, it can only and all of them the the values and types declared on the columns param
 only_cols = FALSE,
 empty_ok = TRUE,
 null_ok = TRUE,
 anyNA = FALSE
)
```


## SF


### SFG


```R
geotypes::sf_sfg(
 #The geometry can only be this geometry types
 #If empty can be any geometry
 types = c("LINESTRING", "POLYGON")
)
```


### SFC


```R
geotypes::sf_sfc(
 #Can only be a point or multilinestring
 #If empty can contain any geometries
 types = c("POINT", "MULTILINESTRING")
)
```


### SF


```R
geotypes::sf_sf(
 #All the params here, will be parsed equally to Data.frame assertion
 #check that one
 df_opts = list(
   columns = list(
     id = typed::Integer(),
     contour = geotypes::sf_sfc(types = "LINESTRING"),
     poly = geotypes::sf_sfc(types = "POLYGON")
   )
 ),
 #The active column must be "geom"
 active_column = "geom",
 #The active column can only be multilinetring of multipolygon
 active_types = c("MULTILINESTRING", "MULTIPOLYGON")
)
```


You can have multiple geometry columns, and specify what each of them is.


## sfnetworks


### sfnetwork


```R
geotypes::sfnetworks_sfnetwork(null_ok = FALSE)
```

