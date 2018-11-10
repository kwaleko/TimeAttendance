module Core.Types where

import  Data.Time


type FromTime = LocalTime
type ToTime   = LocalTime
type Date     = Day



newtype EmpNumber = EmpNumber
  {unEmp :: String}
  deriving(Eq,Show)

data LogType = In | Out
  deriving(Eq,Show)

data LogInRecord = LogInRecord
  {logInEmpNumber :: EmpNumber
  ,logInDateTime  :: LocalTime
  }deriving(Eq,Show)

data LogOutRecord = LogOutRecord
  {logOutEmpNumber :: EmpNumber
  ,logOutDateTime  :: LocalTime
  }deriving(Eq,Show)

data AttendanceRecord = AttLogIn LogInRecord | AttLogOut LogOutRecord
  deriving(Eq,Show)

--data AttendanceRecord = AttendanceRecord
--  {attEmployeeNumber   :: EmpNumber
--  ,attLogType          :: LogType
--  ,attLocalLogDateTime :: LocalTime}
--  deriving(Eq,Show)

data Violation = Violation
  {vioEmployee    :: EmpNumber
  ,vioType        :: ViolationType
  ,vioLocalDate   :: Day
  ,vioFromTime    :: TimeOfDay
  ,vioToTime      :: TimeOfDay}
  deriving(Eq,Show)

data ViolationType =
    None
  | LateArrival
  | EarlyLeave
  | MissingLogIn
  | MissingLogOut
  | DoubleLogIn
  | DoubleLogOut
  | Absence
  deriving(Eq,Show)

data Shift = Shift
  {shfUName         :: String
  ,shfFromTime      :: TimeOfDay
  ,shfToTime        :: TimeOfDay
  ,shfLateCheckIn   :: TimeOfDay
  ,shfEalryCheckOut :: TimeOfDay}
  deriving(Eq,Show)

data Employee = Employee
  {empNumber :: EmpNumber
  ,empName   :: Name
  ,empShift  :: Shift}
  deriving(Eq,Show)

data Name = Name
  {namFirst  :: String
  ,namSecond :: String
  ,namLast   :: String}
  deriving(Eq,Show)

  
