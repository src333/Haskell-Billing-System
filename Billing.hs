module Billing where

import Data.Map (Map,member)
import qualified Data.Set as Set
import qualified Data.Map as Map

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Int
    deriving (Show)

-- Prices for some products
type PriceList = Map Product Double


-- part 1
-- A list without repetitions of all the products that have been ordered.
products :: [Order] -> [Product]
products orders =
    let
        productList = map (\(Order _ product _) -> product) orders
        --  apply our anonymous function to every element product in list orders
    in
        Set.toList (Set.fromList productList)
        -- we convert the list of products into a set which automatically doesnt allow duplications , then convert back into a list , also ive used the let/in format because its a habbit ive picked up from data-visualisation module whihc used functional programming 

-- here how i tested this , i initally planed to use nub but this provides a complexity of o(n)^2
-- terminal cd to directory 
-- ghci 
-- :l file 
-- let orders = [Order "sahar" "pc" 3, Order "dad" "ps4" 2, Order "sahar" "pc" 1]
-- let testq1 = products orders 
-- print testq1
-- or just simply write - products orders 
-- outcome ["pc","ps4"]
-- alternative way to write it - Set.toList (Set.fromList map (\(Order _ product _) -> product) orders) but can be hard to read hence the above format



--part2 
-- a list of products that have been ordered but were not in the price list,
-- each with a list of the customers who ordered them (without repetitions).
unavailable :: [Order] -> PriceList -> [(Product, [Customer])]
unavailable orders prices =
    let
        -- Check if a product is not in the price list
        isUnavailable :: Product -> Bool
        isUnavailable product = not (Map.member product prices)

        -- Collect customers for each unavailable product , allow us to later extract a product and customer pair from this orders list 
        unavailableOrders = [(product, customer) | Order customer product _ <- orders, isUnavailable product]

        -- Group customers by product and create a list of these customers with no duplications 
        groupByProduct :: [(Product, Customer)] -> [(Product, [Customer])]

        -- foldr iterates though a map 'acc' that starts off as empty and updating it by adding product as its key
        -- use set.singleton customer to create a set of customers as values to the key where duplicates can be combined using a .union method
        -- breakdown: 
        -- interation 2 - input is ("pc" ,"sahar") | acc (map) = ("ps4","dad") | Map.insertWith Set.union "PS4" (Set.singleton "Dad") Map.empty -> result Map.fromList [("PS4", Set.fromList ["Dad"])] 
        -- interation 4 - input is ("ps4" ,"dad") | acc (updated map) = (("ps4" , ["dad" , "mom"]) , ("pc", "sahar")) | Map.insertWith Set.union "PS4" (Set.singleton "Dad") acc -> result = Map.fromList [("PS4", Set.fromList ["Dad", "Mom"]), ("PC", Set.fromList ["Sahar"])]
        -- now that the map is built we convert it into a list of tuples using map.tolist and set.tolist
        -- map.tolit convert this | result = Map.fromList [("PS4", Set.fromList ["Dad", "Mom"]), ("PC", Set.fromList ["Sahar"])] -> to this  | result = [("PS4", Set.fromList ["Dad", "Mom"]), ("PC", Set.fromList ["Sahar"])]
        -- set.tolist coverts the above result to this | result = [("PS4", ["Dad", "Mom"]), ("PC", ["Sahar"])]

        groupByProduct ordersGrouped =
            let
                toSetMap = foldr (\(product, customer) acc -> 
                                  Map.insertWith Set.union product (Set.singleton customer) acc)
                                Map.empty ordersGrouped
            in
                [(product, Set.toList customers) | (product, customers) <- Map.toList toSetMap]
    in
        groupByProduct unavailableOrders
    
-- here how i tested this
-- terminal cd to directory 
-- ghci 
-- :l file 
-- let orders = [Order "Sahar" "PC" 3, Order "Dad" "PS4" 2, Order "Mom" "PS4" 1, Order "Dad" "PS4" 1]
-- let prices = Map.fromList [("PC", 10.0)]
-- simpily write - unavailable orders prices
-- outcome [("PS4", ["Dad", "Mom"])] - a list of products with no associated price linked to a list of all customers who bought it with no duplications 



--part3 
-- input are two lists orders and pricelist that contain values to form the out tuple of customer and double where (double = pricelist(map _ double) * order( _ _ quantity))
-- a list of customers who ordered products in the price list,
-- together with the total value of the products they ordered.
bill :: [Order] -> PriceList -> [(Customer, Double)]

-- ordervalue uses map.lookup to check if product exists in the pricelist list 
bill orders prices = 
    let 
        -- calculate the value of each order if the product is in the price list
        orderValue :: Order -> Maybe (Customer, Double)
        orderValue (Order customer product quantity) =
            -- looks up product in the pricelist and returns just price if it exists in the map else nothing if it doesnt
            case Map.lookup product prices of
                Just price -> Just (customer, fromIntegral quantity * price)
                Nothing -> Nothing
         -- collect all valid (customer, value) pairs from list orders 
         -- ordervalue is applied to each order in orders list , if it return just(customer,value) itll be added to the result , else if nothing its skipped 
        validOrders :: [(Customer, Double)]
        validOrders = concatMap (\order -> maybeToList (orderValue order)) orders

        -- Group and sum values by customer
        groupByCustomer :: [(Customer, Double)] -> Map Customer Double
        groupByCustomer = foldr (\(customer, value) acc ->
                                 Map.insertWith (+) customer value acc)
                                Map.empty
    in 
        Map.assocs (groupByCustomer validOrders)
  where
    -- Convert Maybe to a list
    maybeToList (Just x) = [x]
    maybeToList Nothing = []


-- here how i tested this
-- terminal cd to directory 
-- ghci 
-- :l file 
-- let orders = [Order "sahar" "pc" 3, Order "dad" "ps4" 1, Order "sahar" "ps4" 2, Order "mum" "burger" 5]
-- let prices = Map.fromList [("pc", 10.0), ("ps4", 15.0)]  
-- bill orders prices
-- result : [("dad",15.0),("sahar",60.0)]




-- Part 4
-- Like bill, but applying a "buy one, get one free" discounting policy.
-- i.e., if a customer orders 4 of a given product, they pay for 2;
-- if they order 5, they pay for 3.
bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount orders prices = 

    let
        -- Calculate the effective quantity under the discount policy
        -- This function calculates how many items a customer pays for under the "buy one, get one free" rule.
        -- For n items ordered, the customer pays for (n + 1) `div` 2 items.
        -- Examples:
        -- discountedQuantity 4 = (4 + 1) `div` 2 = 2
        -- discountedQuantity 5 = (5 + 1) `div` 2 = 3
        discountedQuantity :: Int -> Int
        discountedQuantity n = (n + 1) `div` 2  

        -- Calculate the value of an order with the discount applied
        -- This function determines the total value of a single order.
        -- It checks if the product exists in the price list using `Map.lookup`.
        -- If the product exists, it calculates the cost based on the discounted quantity and price.
        -- If the product doesn't exist, the order is ignored (returns Nothing).
        -- Example: 
        -- An order for 4 items of a product priced at 10.0 will calculate:
        -- (4 + 1) `div` 2 = 2 items paid for -> 2 * 10.0 = 20.0
        orderValue :: Order -> Maybe (Customer, Double)
        orderValue (Order customer product quantity) =
            case Map.lookup product prices of
                Just price -> Just (customer, fromIntegral (discountedQuantity quantity) * price)
                Nothing -> Nothing  -- Ignore orders for unavailable products

        -- Collect all valid discounted orders
        -- This step generates a flat list of (Customer, Value) pairs for all valid orders.
        -- It uses concatMap with `orderValue` to process each order.
        -- Example:
        -- If we have orders like [Order "sahar" "pc" 4, Order "dad" "ps4" 5]:
        -- Assuming "pc" costs 10.0 and "ps4" costs 15.0, this will generate:
        -- [("sahar", 20.0), ("dad", 45.0)].
        validOrders :: [(Customer, Double)]
        validOrders = concatMap (\order -> maybeToList (orderValue order)) orders

        -- Group and sum values by customer
        -- This step consolidates the (Customer, Value) pairs by customer.
        -- It uses a `Map` to accumulate values for each customer.
        -- If a customer already exists in the map, their value is added to the existing value.
        -- Example:
        -- Input: [("sahar", 20.0), ("sahar", 30.0), ("dad", 45.0)]
        -- Result: Map.fromList [("sahar", 50.0), ("dad", 45.0)].
        groupByCustomer :: [(Customer, Double)] -> Map Customer Double
        groupByCustomer = foldr (\(customer, value) acc ->
                                 Map.insertWith (+) customer value acc)
                                Map.empty
    in
        -- Convert the grouped results from the Map back into a list of tuples.
        -- Example:
        -- Map.fromList [("sahar", 50.0), ("dad", 45.0)]
        -- Result: [("sahar", 50.0), ("dad", 45.0)].
        Map.assocs (groupByCustomer validOrders)
  where
    -- Convert Maybe to a list
    -- Helper function to handle Maybe values.
    -- Converts `Just x` into a single-element list `[x]`.
    -- Converts `Nothing` into an empty list `[]`.
    -- Example:
    -- maybeToList (Just ("Alice", 20.0)) -> [("Alice", 20.0)]
    -- maybeToList Nothing -> []
    maybeToList :: Maybe a -> [a]
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

-- here how i tested this
-- terminal cd to directory 
-- ghci 
-- :l file 
-- let orders = [Order "sahar" "pc" 4, Order "dad" "ps4" 5, Order "mum" "ps5" 3]
-- let prices = Map.fromList [("pc", 10.0), ("ps4", 15.0)]
-- bill_discount orders prices
-- result: [("dad",45.0),("sahar",20.0)]











