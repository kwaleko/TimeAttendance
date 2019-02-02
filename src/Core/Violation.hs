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
  getLogs             :: EmpNumber -> Day -> m [AttendanceRecord]
  getLog              :: EmpNumber -> Day -> Order -> m (Maybe AttendanceRecord)
  getFstLogInByDate   :: EmpNumber -> Day -> m (Maybe LogInRecord)
  getLstLogOutByDate  :: EmpNumber -> Day -> m (Maybe LogOutRecord)

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

genDoubleCheckVio :: (HasViolation m,HasAttendance m) => EmpNumber -> Date -> m (Maybe Violation)
genDoubleCheckVio emp date = do
  mFstLog     <- getLog emp date First
  mLstLog     <- getLog emp date Last
  notViolated <- not <$> doesViolationExists emp date DoubleLog
  return $ do
    fstLog <- mFstLog
    lstLog <- mLstLog
    case (fstLog,lstLog,notViolated) of
      (AttLogIn log,AttLogIn   _ ,True) -> Just $ Violation emp DoubleLog  date (Just $ getLogInTime log ) Nothing
      (AttLogOut _ ,AttLogOut log,True) -> Just $ Violation emp DoubleLog date Nothing (Just $ getLogOutTime log)
      _                                 -> Nothing

{- getMissingLogVio :: (HasViolation m,HasAttendance m,HasCalendar m) => EmpNumber -> Date  -> m (Maybe Violation)
getMissingLogVio emp date = do
  logs       <- getLogs emp date
  isViolated <- doesViolationExists emp date MissingLog
  let missingIn  = hasMissingCheck In logs
      missingOut = hasMissingCheck Out logs
      in return $ case (isViolated,missingIn,missingOut) of
                    (True,True,_) -> Just $ Violation emp MissingLog date Nothing Nothing
                    (True,_,True) -> Nothing
                    _             -> Nothing
    -}
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

getLogInTime :: LogInRecord -> Time
getLogInTime = localTimeOfDay . logInDateTime

getLogOutTime = localTimeOfDay . logOutDateTime
