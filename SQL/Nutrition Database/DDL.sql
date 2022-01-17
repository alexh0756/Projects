drop table food;
drop table nutrient_type;
drop table nutrition_content;
drop table foodintake;

create table food (
    foodKey char(8) NOT NULL,
    foodName varchar(100),
    description varchar(100),
    classification varchar(100),
    scientificname varchar(50),
    details varchar(150),
        primary key (foodKey)
    );

create table nutrient_type (
    nutriName varchar(150) not null,
    nutriType varchar(30),
    measurementUnit varchar(2),
    description varchar(1000),
        primary key (nutriName)
    );

create table nutrition_content (
    foodKey char(8) not null,
    nutriName varchar(15) not null,
    nutriValue integer,
        primary key (foodKey, nutriName)
        foreign key (foodKey)
            references food (foodKey)
        foreign key (nutriName)
            references nutrient_type (nutriName)
    );

drop table location;
create table location (
    postcode integer not null,
    area varchar(30),
    state,
    primary key (postcode));

drop table meal;
create table meal (
    mealID integer not null,
    timestamp text,
    individualID integer,
    postcode integer,
    primary key (mealID)
    foreign key (individualID)
        references individual (individualID)
    foreign key (postcode)
        references location (postcode)
);

create table individual (
    individualID integer not null,
    givenName varchar(20),
    famName varchar(20),
    age integer,
    sex char(1),
    weight integer,
    height integer,
    primary key (individualID)
);

create table foodintake (
    mealID integer not null,
    foodKey char(8) not null,
    quantity float,
    primary key (mealID, foodKey)
    foreign key (mealID)
        references meal (mealID)
    foreign key (foodkey)
        references food (foodKey)
    );
    
    


/* look up the foodkeys from the foodkeyreference table to add data to the foodintake 
table */
select publicfoodkey, name, description from foodkeyreference where name = "Pasta";
select publicfoodkey, name, description from foodkeyreference where name = "Tomato";
select publicfoodkey, name, description from foodkeyreference where name = "Beef";

select publicfoodkey, name, description from foodkeyreference where name like "%beef%";
select publicfoodkey, name, description from foodkeyreference where name like "%chicken%";
select publicfoodkey, name, description from foodkeyreference where name like '%eggplant%';
select publicfoodkey, name, description from foodkeyreference where description like '%grill%';

select publicfoodkey, name, description from foodkeyreference where name like "%soft%";
select * from foodkeyreference where name like '%rice%';
select publicfoodkey, name, description from foodkeyreference where name like '%water%';

select * from location where postcode = 3088;

insert into meal (mealID, timestamp, individualID, postcode)
values (1, "2021-04-09 18:00", 1, 1466),
        (2, "2021-04-09 18:00", 2, 1466),
        (3, "2021-04-10 18:15", 1, 2000),
        (4, "2021-04-10 18:15", 2, 2000),
        (5, "2021-04-11 17:30", 5, 3000),
        (6, "2021-04-12 19:30", 5, 3052),
        (7, "2021-04-12 19:30", 6, 3052),
        (8, "2021-04-12 20:00", 10, 3088),
        (9, "2021-04-12 20:00", 9, 1740),
        (10, "2021-04-12 20:00", 10, 3088),
        (11, "2021-04-12 20:00", 9, 1640);

/* adding pasta */
insert into foodintake (timestamp, foodkey, quantity, units, postcode)
values ("2021-04-09 18:00", "F006431", 150.0, "g", 1466);
/* adding tomato and beef mince */
insert into foodintake (mealID, foodKey, quantity)
values (1, "F009211", 300.0),
    (1, "F006431", 150.0),
    (1, "F000678", 100.0),
    (1, "F009516", 450.0),
    (2, "F009211", 450.0),
    (2, "F006431", 200.0),
    (2, "F000678", 150.0),
    (2, "F009516", 200.0),
    (3, "F008125", 200.0),
    (3, "F006535", 175.0),
    (3, "F002265", 150.0),
    (3, "F005694", 375.0),
    (4, "F008125", 300.0),
    (4, "F006535", 250.0),
    (4, "F002265", 200.0),
    (4, "F005694", 150.0),
    (5, "F002568", 150.0),
    (5, "F003760", 250.0),
    (5, "F004889", 100.0),
    (5, "F002893", 5.0),
    (5, "F009516", 400.0),
    (6, "F002596", 150.0),
    (6, "F007661", 250.0),
    (6, "F002882", 150.0),
    (6, "F003327", 5.0),
    (6, "F009335", 5.0),
    (6, "F008438", 600.0),
    (7, "F002596", 150.0),
    (7, "F007661", 250.0),
    (7, "F002882", 150.0),
    (7, "F003327", 5.0),
    (7, "F008438", 375.0),
    (8, "F000826", 200.0),
    (8, "F002246", 150.0),
    (8, "F009211", 75.0),
    (9, "F006665", 250.0),
    (9, "F004142", 200.0),
    (10, "F002595", 150.0),
    (10, "F002880", 150.0),
    (10, "F003337", 20.0),
    (10, "F007661", 200.0),
    (11, "F008825", 100.0),
    (11, "F003907", 200.0),
    (11, "F007193", 200.0);

select * from foodintake;

insert into individual (individualID, givenName, famName, age, sex, weight, height)
values  (1, 'Peter', 'Henry', 45, 'M', 70, 170),
        (2, 'Lewis', 'Henry', 41, 'M', 90, 180),
        (3, 'Rebecca', 'Henry', 16, 'F', 55, 160),
        (4, 'Kira', 'Henry', 13, 'F', 40, 140),
        (5, 'Dylan', 'Trafford', 31, 'M', 95, 190),
        (6, 'Deloris', 'Trafford', 31,'F', 70, 175),
        (7, 'Michael', 'James', 25, 'M', 82, 178),
        (8, 'Debbie', 'Alban', 19, 'F', 50, 165),
        (9, 'Arthur', 'Tanzi', 27, 'M', 87, 185),
        (10, 'Quintin', 'Esmerelda', 26, 'M', 84, 175),
        (11, 'Claudia', 'Oswin', 23, 'F', 60, 168);