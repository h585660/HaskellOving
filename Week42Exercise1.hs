module Week42Exercise1 where

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (leftFunction, rightFunction)
    where 
        leftFunction a = f (Left a)
        rightFunction b = f (Right b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g eitherValue = 
    case eitherValue of
        Left a  -> f a
        Right b -> g b

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (functionForFirst, functionForSecond)
    where
        functionForFirst a = fst (f a)
        functionForSecond a = snd (f a)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g input = 
    let result1 = f input
        result2 = g input
    in (result1, result2)