module Core.Violation where

import Core.Types

import                 Control.Monad
import                 Data.Bool          (bool)
import                 Data.Time          (Day)
import                 Data.Time.LocalTime(LocalTime
                                          ,localTimeOfDay)


class Monad m => HasViolation m where
  createViolation        :: Violation -> m ()
  doesViolationExists    :: EmpNumber -> Day -> ViolationType -> m Bool
  doesDoubleLogVioExists :: EmpNumber -> Day -> m Bool

class Monad m => HasShiftSetup m where
  getShifts    :: m [Shift]
  getEmpShift  :: EmpNumber -> m (Maybe Shift)

class Monad m => HasAttendance m where
  getAttendanceRecs   :: EmpNumber -> Day -> m [AttendanceRecord]
  getFstLogInByDate   :: EmpNumber -> Day -> m (Maybe LogInRecord)
  getLstLogOutByDate  :: EmpNumber -> Day -> m (Maybe LogOutRecord)


genLateArrivalVio :: (HasAttendance m,HasViolation m,HasShiftSetup m) => EmpNumber -> Date -> m (Maybe Violation)
genLateArrivalVio emp date = do
  fst        <- getFstLogInByDate emp date
  shift      <- getEmpShift emp
  isViolated <- doesViolationExists emp date LateArrival
  return $ do
    logIn  <- fst
    shf    <- shift
    let fromTime      = Just $ shfFromTime shf
        toTime        = Just $ (localTimeOfDay . logInDateTime) logIn
        isNotViolated = not isViolated
        isLate        = isLateArrival logIn shf
    bool
      Nothing
      (Just $ Violation emp EarlyLeave date fromTime toTime)
      (isNotViolated && isLate)

isLateArrival :: LogInRecord -> Shift -> Bool
isLateArrival logRec shft =
  (localTimeOfDay . logInDateTime) logRec > shfLateCheckIn shft

isEarlyLeave :: LogOutRecord -> Shift -> Bool
isEarlyLeave logRec shft =
  (localTimeOfDay . logOutDateTime) logRec < shfEalryCheckOut shft

hasMissingCheck :: LogType -> [AttendanceRecord] -> Bool
hasMissingCheck logType attRec = case attRec of
  []   -> False
  recs -> length (filter fn attRec) == 0
  where
    fn :: AttendanceRecord -> Bool
    fn x = case x of
      AttLogIn  _ ->  logType == In
      AttLogOut _ ->  logType == Out

hasMissingCheckIn :: [AttendanceRecord] -> Bool
hasMissingCheckIn = hasMissingCheck In

hasMissingCheckOut :: [AttendanceRecord] -> Bool
hasMissingCheckOut = hasMissingCheck Out

attendanceType :: AttendanceRecord -> LogType
attendanceType record = case record of
  AttLogIn  _ -> In
  AttLogOut _ -> Out

--hasDoubleCheck :: (Maybe AttendanceRecord,Maybe AttendanceRecord) ->  Maybe ViolationType
--hasDoubleCheck  (Just (AttLogIn login)  ,Just (AttLogOut logout)) = Nothing
--hasDoubleCheck  (Just (AttLogIn _)       ,Just (AttLogIn  _     )) = Just DoubleLogIn
--hasDoubleCheck  (Just (AttLogOut _)      ,Just (AttLogOut _     )) = Just DoubleLogOut
--hasDoubleCheck  (Nothing                 ,Just  _                ) = Just MissingLogIn
--hasDoubleCheck  (Just _                  ,Nothing                ) = Just MissingLogOut
--hasDoubleCheck    _                                                = Nothing


