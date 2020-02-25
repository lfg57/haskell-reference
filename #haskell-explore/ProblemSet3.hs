-- [4]
-- =============================================================================
-- Bubble Sort implementation.
-- =============================================================================

-- =============================================================================
-- Solution:
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort []  = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = 
    let (smaller,bigger) = if x > y then (y,x) else (x,y)    --compare adj
        split = smaller : bubbleSort (bigger:xs)
    in bubbleSort (init split) ++ [last split]
-- =============================================================================

-- Tests:

--bubbleSort [6,1,3,2,5]    -- [1,2,3,5,6]