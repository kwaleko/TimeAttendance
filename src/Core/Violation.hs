module Core.Violation where

import Core.Types

import                 Control.Monad
import                 Data.Time          (Day)
import                 Data.Time.LocalTime(LocalTime
                                          ,localTimeOfDay)


class Monad m => HasViolation m where
  createViolation     :: Violation -> m ()
  doesViolationExists :: EmpNumber -> Day -> ViolationType -> m Bool

class Monad m => HasShift m where
  getShifts    :: m [Shift]
  getEmpShift  :: EmpNumber -> m (Maybe Shift)

class Monad m => HasAttendance m where
  getAttendanceRecs   :: m [AttendanceRecord]
  getAttendanceRecBy  :: EmpNumber -> QueryBy -> m [AttendanceRecord]
  getFstLogInByDate   :: EmpNumber -> Day     -> m (Maybe LogInRecord)
  getLstLogOutByDate  :: EmpNumber -> Day     -> m (Maybe LogOutRecord)

data QueryBy =
    All
  | QBDate    Day
  | QBLogType LogType

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
