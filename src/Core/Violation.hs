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
  getLog              :: EmpNumber -> Day -> Order -> m (Maybe AttendanceRecord)

data Order = First | Last

class Monad m => HasPermission m where
  doesEarlyLeavePermExists :: EmpNumber -> Day -> m Bool

class Monad m => HasCalendar m where
  isHoliday :: Day -> m (Maybe Bool)

class Monad m => HasReducedSchedule m where
  doesReducedSchedExists :: EmpNumber -> Date -> m Bool

genLateArrivalVio :: (HasAttendance      m
                     ,HasViolation       m
                     ,HasShiftSetup      m
                     ,HasPermission      m
                     ,HasCalendar        m
                     ,HasReducedSchedule m)
                  => EmpNumber -> Date -> m (Maybe Violation)
genLateArrivalVio emp date = do
  fst           <- getFstLogInByDate emp date
  shift         <- getEmpShift emp
  isNotViolated <- not <$> doesViolationExists emp date LateArrival
  noPermission  <- not <$> doesEarlyLeavePermExists emp date
  noReducedSchd <- not <$> doesReducedSchedExists emp date
  holiday       <- isHoliday date
  return $ do
    logIn  <- fst
    shf    <- shift
    isNotHoliday <- not <$> holiday
    let fromTime      = Just $ shfFromTime shf
        toTime        = Just $ (localTimeOfDay . logInDateTime) logIn
        isLate        = isLateArrival logIn shf
    bool
      Nothing
      (Just $ Violation emp EarlyLeave date fromTime toTime)
      (isNotViolated && isLate && noPermission && isNotHoliday && noReducedSchd)

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

genDoubleCheckVio :: (HasViolation m,HasAttendance m,HasCalendar m) => EmpNumber -> Date -> m (Maybe Violation)
genDoubleCheckVio = undefined
  

--isDoubleCheck :: (AttendanceRecord,AttendanceRecord) -> Bool
--hasDoubleCheck :: (Maybe AttendanceRecord,Maybe AttendanceRecord) ->  Maybe ViolationType
--hasDoubleCheck  (Just (AttLogIn login)  ,Just (AttLogOut logout)) = Nothing
--hasDoubleCheck  (Just (AttLogIn _)       ,Just (AttLogIn  _     )) = Just DoubleLogIn
--hasDoubleCheck  (Just (AttLogOut _)      ,Just (AttLogOut _     )) = Just DoubleLogOut
--hasDoubleCheck  (Nothing                 ,Just  _                ) = Just MissingLogIn
--hasDoubleCheck  (Just _                  ,Nothing                ) = Just MissingLogOut
--hasDoubleCheck    _                                                = Nothing
