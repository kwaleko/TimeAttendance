module ViolationSpec(spec) where


import          Core.Arbitrary
import          Core.Types
import          Core.Violation


import          Data.Bool(bool)
import          Data.Time.LocalTime(localTimeOfDay)
import          Test.Hspec
import          Test.QuickCheck

spec :: Spec
spec = do
  describe "isLateArrival" $ do
    it "should return true when the attendance logIn date is later than the shift maximum late checkIn time" $ property $
      \logInRec shift ->
        let logIn          = localTimeOfDay $ logInDateTime logInRec
            maxLateArrival = shfLateCheckIn shift
            in logIn > maxLateArrival ==>
               isLateArrival logInRec shift `shouldBe` True

  describe "isLateArrival" $ do
    it "should return false when the attendance logIn date is earlier than the shift maximum late checkIn time" $ property $
      \logInRec shift ->
        let logIn          = localTimeOfDay $ logInDateTime logInRec
            maxLateArrival = shfLateCheckIn shift
            in logIn < maxLateArrival ==>
               isLateArrival logInRec shift `shouldBe` False

  describe "isEarlyLeave" $ do
    it "should return true when the attendance logOut date is earlier than the shift minimum early checkOut time" $ property $
      \logOutRec shift ->
        let logOut        = localTimeOfDay $ logOutDateTime logOutRec
            minEarlyLeave = shfEalryCheckOut shift
            in logOut < minEarlyLeave ==>
               isEarlyLeave logOutRec shift `shouldBe` True

  describe "isEarlyLeave" $ do
    it "should return false when the attendance logOut date is later than the shift minimum early checkOut time" $ property $
      \logOutRec shift ->
        let logOut        = localTimeOfDay $ logOutDateTime logOutRec
            minEarlyLeave = shfEalryCheckOut shift
              in logOut > minEarlyLeave ==>
                 isEarlyLeave logOutRec shift `shouldBe` False

  describe "hasMissingCheckIn" $ do
    it "should return false for a given day when the employee has no record in the attendance, not absent" $ property $
        hasMissingCheckIn [] `shouldBe` False

  describe "hasMissingCheckIn" $ do
    it "should return False for a given day when the employee has at least one record checkIn" $ property $
      \attRecs ->
        let res              = map attendanceType attRecs
            checkInAvailable = (== In) `any` res
            in checkInAvailable ==>
              hasMissingCheckIn attRecs `shouldBe` False

  describe "hasMissingCheckIn" $ do
    it "should return true when the employee has no record for logIn while having logOut records" $ property $
      \attRecs ->
        let res               = map attendanceType attRecs
            onlyCheckOut  = filter (== Out)  res
            in case attRecs of
                 [] -> hasMissingCheckIn attRecs `shouldBe` False
                 x -> bool
                      (hasMissingCheckIn x `shouldBe` False )
                      (hasMissingCheckIn x `shouldBe` True  )
                      (length attRecs == length onlyCheckOut)

  describe "hasMissingCheckOut" $ do
    it "should return false for a given day when the employee has no record in the attendance, not absent" $ property $
        hasMissingCheckOut [] `shouldBe` False

  describe "hasMissingCheckOut" $ do
    it "should return False for a given day when the employee has at least one record checkOut" $ property $
      \attRecs ->
        let res               = map attendanceType attRecs
            checkOutAvailable = (== Out) `any` res
            in checkOutAvailable ==>
              hasMissingCheckOut attRecs `shouldBe` False

  describe "hasMissingCheckOut" $ do
    it "should return true when the employee has no record for logOut while having logIn records" $ property $
      \attRecs ->
        let res               = map attendanceType attRecs
            onlyCheckIn  = filter (== In)  res
            in case attRecs of
                 [] -> hasMissingCheckOut attRecs `shouldBe` False
                 x -> bool
                      (hasMissingCheckOut x `shouldBe` False )
                      (hasMissingCheckOut x `shouldBe` True  )
                      (length attRecs == length onlyCheckIn)

{-  describe "hasDoubleCheck" $ do
    it "should return Nothing when the employee has the first log type as checkIn and the last log type as checkOut" $ property $
      \(fstLog,lstLog) ->
        attendanceType fstLog == In &&
        attendanceType lstLog == Out ==>
         hasDoubleCheck (fstLog,lstLog) `shouldBe` Nothing

  describe "hasDoubleCheck" $ do
    it "should return a double checkIn violation when both the first check and last check were both of type checkIn" $ property $
      \(fstLog,lstLog) ->
        attendanceType fstLog == In &&
        attendanceType lstLog == In ==>
        hasDoubleCheck (fstLog,lstLog) `shouldBe` Just DoubleLogIn

  describe "hasDoubleCheck" $ do
    it "should return a violation of type double CheckOut when the first and last log are both of type checkOut" $ property $
      \(fstLog,lstLog) ->
        attendanceType fstLog == Out &&
        attendanceType lstLog == Out ==>
        hasDoubleCheck (fstLog,lstLog) `shouldBe` Just DoubleLogOut
-}
