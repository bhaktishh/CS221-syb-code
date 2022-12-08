module Work
    ( main'
    ) where

import Data.Data
import Data.Generics.Schemes
import Data.Generics.Aliases

data Class = Class ClassName Dept Code Professor [Student] deriving (Typeable, Data, Read, Show)
data Person = Person FirstName LastName deriving (Typeable, Data, Read, Show)
data Student = Student Person Grade deriving (Typeable, Data, Read, Show)
type Professor = Person
type FirstName = String 
type LastName = String 
type ClassName = String
data Dept = CMSC | MATH | ETC deriving (Typeable, Data, Read, Show)
type Code = Int
data Grade = A_plus | A | A_minus | B_plus | B | B_minus | C_plus | C | C_minus | F deriving (Typeable, Data, Read, Show)

ashaw :: Professor
ashaw = Person "Adam" "Shaw"

achurch, agaubil, bshah, dchen, emusk, thoare :: Student
achurch = Student (Person "Alonzo" "Church") A_plus
agaubil = Student (Person "Alexandre" "Gaubil") A
bshah = Student (Person "Bhakti" "Shah") B
dchen = Student (Person "David" "Chen") A_minus
emusk = Student (Person "Elon" "Musk") F
thoare = Student (Person "Sir Antony" "Hoare") A_plus

-- let's pretend like there are more

plClass :: Class
plClass = Class "Programming Languages" CMSC 22100 ashaw [achurch, agaubil, bshah, dchen, emusk, thoare]

gradeInc :: Grade -> Grade
gradeInc A = A_plus
gradeInc B = B_plus
gradeInc C = C_plus
gradeInc A_minus = A
gradeInc B_minus = B
gradeInc C_minus = C
gradeInc x = x

gradeAplus :: Grade -> Grade
gradeAplus _ = A_plus

increase :: Class -> Class
increase = everywhere (mkT gradeInc)

increase' :: Data a => a -> a
increase' = everywhere (mkT gradeAplus)

increaseSelfish :: GenericT
increaseSelfish p
    | isPerson "Alexandre" p = increase' p
    | isPerson "Bhakti" p = increase' p
    | isPerson "David" p = increase' p
    | otherwise = gmapT increaseSelfish p

isPerson :: FirstName -> GenericQ Bool
isPerson n = mkQ False (isPersonS n)

isPersonS :: FirstName -> Student -> Bool
isPersonS n (Student (Person fn _) _) = n==fn

main' :: IO ()
main' = do 
    print $ increase plClass
    print $ increaseSelfish plClass
