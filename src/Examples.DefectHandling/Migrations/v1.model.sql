create table Categories
    ( Id guid primary key
    , Name string (64) unique
    );

create table Subcategories
    ( Id guid primary key
    , CategoryId guid references Categories (Id)
    , Name string (64)
    );