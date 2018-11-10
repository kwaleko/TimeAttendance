module Core.Types where

import  Data.Time


type FromTime = TimeOfDay
type ToTime   = TimeOfDay
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
  ,vioFromTime    :: Maybe FromTime
  ,vioToTime      :: Maybe ToTime}
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
  ,shfFromTime      :: FromTime
  ,shfToTime        :: ToTime
  ,shfLateCheckIn   :: FromTime
  ,shfEalryCheckOut :: ToTime}
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

data Period = Period
  {periodId   :: String
  ,periodFrom :: Day
  ,periodTo   :: Day
  ,periodName :: String
  }deriving(Eq,Show)
