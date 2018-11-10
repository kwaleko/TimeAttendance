module Core.Arbitrary where

import Core.Types
import Data.Time
import Data.Time.LocalTime
import Data.Fixed

import Test.QuickCheck


instance Arbitrary LogInRecord where
  arbitrary = LogInRecord
    <$> (arbitrary :: Gen EmpNumber)
    <*> (arbitrary :: Gen LocalTime)

instance Arbitrary LogOutRecord where
  arbitrary = LogOutRecord
    <$> (arbitrary :: Gen EmpNumber)
    <*> (arbitrary :: Gen LocalTime)

instance Arbitrary AttendanceRecord where
  arbitrary = oneof [
      AttLogIn  <$> (arbitrary ::Gen LogInRecord)
    , AttLogOut <$> (arbitrary :: Gen LogOutRecord)
    ]

instance Arbitrary EmpNumber where
  arbitrary = EmpNumber
    <$> (arbitrary :: Gen String)

instance Arbitrary LocalTime where
  arbitrary = LocalTime
    <$> (arbitrary :: Gen Day)
    <*> (arbitrary :: Gen TimeOfDay)

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> (arbitrary :: Gen Integer)


instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay
   <$> (elements [1,2..23])
   <*> (elements [0,1..60])
   <*> (elements [0])

instance Arbitrary Shift where
  arbitrary = Shift
    <$> (arbitrary :: Gen String)
    <*> (arbitrary :: Gen TimeOfDay )
    <*> (arbitrary :: Gen TimeOfDay )
    <*> (arbitrary :: Gen TimeOfDay )
    <*> (arbitrary :: Gen TimeOfDay )

