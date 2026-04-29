! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Tool definition helpers for the Copilot SDK.
!! Provides a convenience subroutine for creating tool descriptors.
module copilot_tools
  use copilot_types
  implicit none
  private

  public :: define_tool, tool_success, tool_failure

contains

  !> Create a copilot_tool descriptor.
  !!
  !! @param name          Tool name visible to the assistant
  !! @param description   Human-readable description of what the tool does
  !! @param schema_json   JSON Schema string describing the tool parameters
  !! @param callback      Procedure pointer invoked when the assistant calls the tool
  !! @return              Populated copilot_tool instance
  function define_tool(name, description, schema_json, callback) result(tool)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: schema_json
    procedure(tool_callback_interface) :: callback
    type(copilot_tool) :: tool

    tool%name = name
    tool%description = description
    tool%input_schema_json = schema_json
    tool%callback => callback
  end function define_tool

  !> Create a successful tool result.
  function tool_success(content) result(res)
    character(len=*), intent(in) :: content
    type(tool_result) :: res

    res%content = content
    res%is_error = .false.
  end function tool_success

  !> Create a failure tool result.
  function tool_failure(error_text) result(res)
    character(len=*), intent(in) :: error_text
    type(tool_result) :: res

    res%error_text = error_text
    res%is_error = .true.
  end function tool_failure

end module copilot_tools
