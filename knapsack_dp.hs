type Item = (Int, Int)

--- Dynamic Programming knapsack solver
knapsack_dp_single :: [Item] -> Int -> Int
knapsack_dp_single itms max_wt = foldr max 0 (knapsack_dp itms max_wt)

knapsack_dp :: [Item] -> Int -> [Int]
knapsack_dp [] max_wt = take max_wt (repeat 0) 
knapsack_dp (item:xs) max_wt = knap_inner item (knapsack_dp xs max_wt)

knap_inner :: Item -> [Int] -> [Int]
knap_inner _ [] = []
knap_inner item@(val, wt) lst@(last_val:xs) = (let len = length lst in 
    (if wt <= len 
        then max 
            (if wt < len then val + lst !! wt else val) 
            last_val 
        else last_val)
    ) : knap_inner item xs

--- Recursive knapsack solver
knapsack_rec :: [Item] -> Int -> Int
knapsack_rec [] _ = 0
knapsack_rec ((value, weight):xs) max_weight = 
    let rest = knapsack_rec xs 
        remain = max_weight - weight
    in max 
        (if remain > 0 then value + rest remain else 0) 
        (rest max_weight)

items :: [Item]
items = [(79, 85), (32, 26), (47, 48), (18, 21), (26, 22), (85, 95), (33, 43), (40, 45), (45, 55), (59, 52)]

main :: IO ()
main = do
    print (knapsack_dp_single items 101)
    print (knapsack_rec items 101)
