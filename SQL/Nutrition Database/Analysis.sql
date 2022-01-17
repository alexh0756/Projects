select * from food;
select * from nutrient_type;
select * from nutrition_content;
select * from location;
select * from meal;
select * from foodintake;

/* Analysis 1 */
select individual.givenName, nutrient_type.nutriName, round(sum((quantity/100)*nutriValue), 2) as total_protein, nutrient_type.description
from meal, foodintake, location, nutrition_content, nutrient_type, individual
where meal.mealID = foodintake.mealID and meal.postcode = location.postcode 
and foodintake.foodKey = nutrition_content.foodKey and nutrition_content.nutriName = nutrient_type.nutriName
and individual.individualID = meal.individualID
and state = 'VIC' and nutrition_content.nutriName like 'protein' and individual.individualID = 5
group by givenName;

/* Analysis 2 */
select individual.givenName, food.foodName, state, nutrient_type.nutriName, quantity, nutriValue as per100gml, (quantity/100)*nutriValue as total_intake, nutrient_type.measurementUnit
from foodintake, nutrition_content, location, nutrient_type, food, individual, meal
where foodintake.mealID = meal.mealID and meal.postcode = location.postcode and foodintake.foodKey = nutrition_content.foodKey 
and nutrition_content.nutriName = nutrient_type.nutriName and foodintake.foodKey = food.foodKey
and meal.individualID = individual.individualID
and state = 'NSW' and nutrient_type.nutriName like 'fat' and individual.individualID = 1
order by nutriValue desc;

select *
from nutrient_type;

/* Analysis 3 */
select timestamp, givenName, foodName, food.description, area
from foodintake, food, individual, location, meal
where foodintake.foodKey = food.foodKey and meal.mealID = foodintake.mealID and individual.individualID = meal.individualID
and meal.postcode = location.postcode
and foodName like 'chicken';

/* Analysis 4 */
select individual.givenName, food.foodName, area, quantity*nutriValue/100 as proteinIntake, nutrient_type.measurementUnit
from foodintake, nutrition_content, location, nutrient_type, food, individual, meal
where meal.postcode = location.postcode and meal.mealID = foodintake.mealID and foodintake.foodKey = nutrition_content.foodKey 
and nutrition_content.nutriName = nutrient_type.nutriName and foodintake.foodKey = food.foodKey
and meal.individualID = individual.individualID and nutrition_content.nutriName like 'protein' 
and nutrition_content.nutriValue > 0;

/* Analysis 5 */
select timestamp, individual.givenName, sum(quantity) as intakeQuant, 
round(avg(nutriValue),2) as avgValue, round(sum(quantity*nutriValue/100),2) as nutritionIntake, nutrient_type.nutriName
from foodintake, nutrition_content, nutrient_type, individual, meal
where foodintake.foodKey = nutrition_content.foodKey
and nutrient_type.nutriName = nutrition_content.nutriName
and individual.individualID = meal.individualID and meal.mealID = foodintake.mealID
and timestamp like '2021-04-09%' and nutriType = 'proximate' and nutriValue > 0 and sex = 'M'
group by nutrient_type.nutriName, meal.individualID
order by givenName;

/* Analysis 6 */
select area, sum(quantity) as foodIntake, round(avg(nutriValue),2) as nutrientsValue, 
measurementUnit as nutrient_untis, round(sum(quantity*nutriValue/100),2) as nutrientIntake, round(avg(age),2) as age, 
round(avg(height),2) as height, round(avg(weight),2) as weight
from foodintake, nutrition_content, location, nutrient_type, individual, meal
where meal.postcode = location.postcode and foodintake.foodKey = nutrition_content.foodKey 
and nutrition_content.nutriName = nutrient_type.nutriName and meal.mealID = foodintake.mealID
and meal.individualID = individual.individualID
and nutrient_type.nutriName like '%sugar%' and nutriValue > 0 and age > 21 and age < 70
group by area;

/* checking answer */
select area, quantity as foodIntake, units, value as nutrientsValue, 
measure, age as age, 
height as height, weight as weight
from foodintake, nutrition_content, location, nutrient_types, individual
where foodintake.postcode = location.postcode and foodintake.foodkey = nutrition_content.foodkey 
and nutrition_content.vitamin = nutrient_types.name
and foodintake.individual = individual.ID
and vitamin like '%sugar%' and value > 0 and age > 21 and age < 70;

/* Analysis 7 */
select givenName, famName, sex, area 
from foodintake, individual, location, meal
where foodKey in (select foodKey from food where classification like '%spices%')
and meal.individualID = individual.individualID and location.postcode = meal.postcode
and meal.mealID = foodintake.mealID
and quantity < 100
group by individual.individualID;

/* Analysis 8 */
select state, round(sum(quantity*nutriValue/100),2) as averageVitaminC, nutrient_type.measurementUnit
from foodintake, nutrition_content, location, nutrient_type, individual, meal
where meal.postcode = location.postcode and foodintake.foodKey = nutrition_content.foodKey 
and nutrition_content.nutriName = nutrient_type.nutriName and meal.mealID = foodintake.mealID
and meal.individualID = individual.individualID
and nutrient_type.nutriName like 'vitamin c' and nutriValue > 0 and age > 21
group by state;

/* checking answer */
select state, quantity, value, nutrient_types.measure
from foodintake, nutrition_content, location, nutrient_types, individual
where foodintake.postcode = location.postcode and foodintake.foodkey = nutrition_content.foodkey 
and nutrition_content.vitamin = nutrient_types.name
and foodintake.individual = individual.ID
and vitamin like 'vitamin c' and value > 0 and age > 21;

/* Analysis 9 */
select GivenName, famName, area, round(sum(quantity*nutriValue/100),2) as averageVitaminD, nutrient_type.measurementUnit
from foodintake, nutrition_content, location, nutrient_type, individual, meal
where meal.postcode = location.postcode and foodintake.foodkey = nutrition_content.foodkey 
and nutrition_content.nutriName = nutrient_type.nutriName and meal.mealID = foodintake.mealID
and meal.individualID = individual.individualID
and nutrient_type.nutriName like 'vitamin d%' and nutriValue > 0 and weight > 60 and weight < 120 and sex = 'M'
group by individual.individualID;

/* Analysis 10 */
select givenName, famName, age
from foodintake, individual, food, meal
where food.foodKey = foodintake.foodKey
and meal.individualID = individual.individualID
and meal.mealID = foodintake.mealID
and food.foodName like '%tomato%'
group by individual.individualID;
