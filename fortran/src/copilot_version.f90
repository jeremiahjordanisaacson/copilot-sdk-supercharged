! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Protocol version constant for the Copilot SDK.
module copilot_version
  implicit none
  private
  public :: SDK_PROTOCOL_VERSION

  character(len=*), parameter :: SDK_PROTOCOL_VERSION = '2'

end module copilot_version
