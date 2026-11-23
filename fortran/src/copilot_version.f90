! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Protocol version constant for the Copilot SDK.
module copilot_version
  implicit none
  private
  public :: SDK_PROTOCOL_VERSION, SDK_PROTOCOL_VERSION_TEXT

  !> SDK protocol version spoken by this SDK; the server must report exactly this version.
  integer, parameter :: SDK_PROTOCOL_VERSION = 4

  !> The SDK protocol version as text (for messages and logs).
  character(len=*), parameter :: SDK_PROTOCOL_VERSION_TEXT = '4'

end module copilot_version
