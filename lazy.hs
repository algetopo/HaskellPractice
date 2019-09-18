import Prelude
--joker program 

--main = putStrLn "Hello, World!"

merge::Ord a=>[a]->[a]->[a]->[a]
merge acum [] ay = acum++ay
merge acum ax []= acum++ax
merge acum (x:xs) (y:ys)=case (x<=y) of 
                                     True->merge accx xs listy
                                     False->merge accy listx ys

     where 
     listx=x:xs
     listy=y:ys
     accx=acum++[x]
     accy=acum++[y]

mergesort::Ord a=>[[a]]->[[a]]
mergesort (x:xs)=lst_out
                 

 where 
      
      x2=head xs
      remainder=tail xs
      lst_out=[merge [] x x2]++remainder
      

mergesort'::Ord a=>[[a]]->[[a]]->[[a]] 
mergesort' accum []=accum 
mergesort' accum (x:xs)
                     |xs/=[]=mergesort' acc remainder
                     |otherwise=accum++[x]
                 
                  
 where 
      --remainder is either [[a]] or [[]]
      acc=accum++[merge [] x  (head xs)]
      remainder=tail xs
      


 





   

 




temporal::[a]->[[a]]
temporal list=fmap (\x->[x]) list

--mergesort[[0,2,6,8] ,[1,3,5,7] ,[11,14,15,17],[1,3,5,7]]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList  (Cons a(x))=a:streamToList x

instance Show a => Show (Stream a) where
         show s= init(show(take 20(streamToList s)))

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)


order :: [Int]->[Int]
order lis= fmap ((\x->x `mod` (13)).(^3))lis 


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s)= Cons(f(x)) (streamMap f s)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate fun a =Cons(fun a)(streamIterate fun (fun a) )


streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a (x)) s= Cons a (streamInterleave s x)


nats :: Stream Integer
nats=streamIterate (\x->x+1) (-1) 



fibs2::[Integer]
fibs2=1:1:zipWith (+) fibs2 (tail fibs2)

trivial::Int->Int
trivial n=n*10


sumsquares::Integer->Integer->Integer
sumsquares 0 acc= acc
sumsquares n acc= sumsquares (n-1) ((n*n)+ acc)




dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2


data List a=Empty | Entry a(List a) deriving(Show)


printList::List a->String->String
printList (Entry a as) s=s++"a"
printList  Empty s=s




mirrorList'::List a->List a->List a
mirrorList' ls (Entry a as)= mirrorList' (Entry a(ls)) as
mirrorList' ls (Empty)=ls
 

ls::List Int
ls=Entry 1(Entry 2 (Empty))




main :: IO ()
main = putStrLn (show(mirrorList' Empty ls))
