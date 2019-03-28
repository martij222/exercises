# refine-exercise
Basic data manipulation exercise using a toy data set showing product purchases from an electronics store. Inspired by [this post.](http://d3-media.blogspot.com/2013/11/how-to-refine-your-data.html)

## Brand names
The *company* column has several spellings for each of the four brands. They are all standardized to have the correct spelling, all lowercase.

## Product code and number
The product code and number column (*pcode_number*) is separated into two variables: *product_code* and *product_number*.

## Product Categories
The product codes represent the following product categories:
- p = Smartphone
- v = TV
- x = Laptop
- q = Tablet

For readability, a column is added with the product category for each observation.

## Full address for geocoding
A new column, *full_address*, is created that concatenates the three original address fields (*address*, *city*, *country*).

## Dummy Variables for Product Category
"Company" and "Product Category" are both categorical variables, so they need dummy variables to be used in further analysis.

Four columns for each company were added:
- *company_philips*
- *company_akzo*
- *company_van_houten*
- *company_unilever*

Four columns for each product category were also created:
- *product_smartphone*
- *product_tv*
- *product_laptop*
- *product_tablet*
