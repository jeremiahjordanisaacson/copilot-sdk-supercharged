! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Derived types for the Copilot SDK.
!! Defines options, configuration, metadata, events, tools, and permissions.
module copilot_types
  implicit none
  private

  ! Re-export all public types
  public :: copilot_client_options, session_config, session_metadata
  public :: session_event, send_options, send_and_wait_result
  public :: copilot_tool, tool_result, tool_invocation
  public :: permission_request, permission_result
  public :: elicitation_request, elicitation_result
  public :: session_fs_config, session_fs_entry
  public :: copilot_connection_state
  public :: mcp_server_config, command_definition, image_options
  public :: COPILOT_STATE_DISCONNECTED, COPILOT_STATE_CONNECTING
  public :: COPILOT_STATE_CONNECTED, COPILOT_STATE_ERROR

  ! --------------------------------------------------------------------------
  ! Connection state enum
  ! --------------------------------------------------------------------------
  integer, parameter :: COPILOT_STATE_DISCONNECTED = 0
  integer, parameter :: COPILOT_STATE_CONNECTING   = 1
  integer, parameter :: COPILOT_STATE_CONNECTED    = 2
  integer, parameter :: COPILOT_STATE_ERROR        = 3

  type :: copilot_connection_state
    integer :: value = COPILOT_STATE_DISCONNECTED
  end type copilot_connection_state

  ! --------------------------------------------------------------------------
  ! Client options
  ! --------------------------------------------------------------------------
  type :: copilot_client_options
    character(len=:), allocatable :: cli_path
    character(len=:), allocatable :: cli_url
    character(len=:), allocatable :: cwd
    character(len=:), allocatable :: log_level
    character(len=:), allocatable :: github_token
    logical :: auto_start       = .true.
    logical :: auto_restart     = .false.
    logical :: use_logged_in_user = .false.
    logical :: use_stdio        = .true.
    integer :: session_idle_timeout_seconds = 0
    type(session_fs_config), allocatable :: session_fs
  contains
    procedure :: set_defaults => client_options_set_defaults
  end type copilot_client_options

  ! --------------------------------------------------------------------------
  ! Session filesystem entry
  ! --------------------------------------------------------------------------
  type :: session_fs_entry
    character(len=:), allocatable :: path
    character(len=:), allocatable :: content
    logical :: is_directory = .false.
  end type session_fs_entry

  ! --------------------------------------------------------------------------
  ! Session filesystem configuration
  ! --------------------------------------------------------------------------
  type :: session_fs_config
    character(len=:), allocatable :: initial_cwd
    character(len=:), allocatable :: session_state_path
    character(len=:), allocatable :: conventions
  end type session_fs_config

  ! --------------------------------------------------------------------------
  ! MCP server configuration
  ! --------------------------------------------------------------------------
  type :: mcp_server_config
    character(len=:), allocatable :: command
    character(len=:), allocatable :: server_type
    character(len=:), allocatable :: url
    integer :: timeout = 0
  end type mcp_server_config

  ! --------------------------------------------------------------------------
  ! Command definition
  ! --------------------------------------------------------------------------
  type :: command_definition
    character(len=:), allocatable :: name
    character(len=:), allocatable :: description
  end type command_definition

  ! --------------------------------------------------------------------------
  ! Image options
  ! --------------------------------------------------------------------------
  type :: image_options
    character(len=:), allocatable :: size
    character(len=:), allocatable :: quality
    character(len=:), allocatable :: style
  end type image_options

  ! --------------------------------------------------------------------------
  ! Session configuration
  ! --------------------------------------------------------------------------
  type :: session_config
    character(len=:), allocatable :: model
    character(len=:), allocatable :: system_prompt
    character(len=:), allocatable :: github_token
    character(len=:), allocatable :: workspace_path
    character(len=:), allocatable :: session_id
    type(copilot_tool), allocatable :: tools(:)
    type(session_fs_config) :: fs
    logical :: streaming = .true.
    integer :: session_idle_timeout_seconds = 0
    character(len=:), allocatable :: skill_directories(:)
    character(len=:), allocatable :: disabled_skills(:)
    character(len=:), allocatable :: excluded_tools(:)
    logical :: include_sub_agent_streaming_events = .false.
    logical :: enable_config_discovery = .false.
    character(len=:), allocatable :: model_capabilities_json
    character(len=:), allocatable :: request_headers_json
    character(len=:), allocatable :: mcp_servers_json
    type(command_definition), allocatable :: commands(:)
    type(image_options) :: img_options
    character(len=:), allocatable :: response_format
  end type session_config

  ! --------------------------------------------------------------------------
  ! Session metadata
  ! --------------------------------------------------------------------------
  type :: session_metadata
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: model
    character(len=:), allocatable :: status
    character(len=:), allocatable :: created_at
    integer :: message_count = 0
  end type session_metadata

  ! --------------------------------------------------------------------------
  ! Session event
  ! --------------------------------------------------------------------------
  type :: session_event
    character(len=:), allocatable :: event_type
    character(len=:), allocatable :: id
    character(len=:), allocatable :: timestamp
    character(len=:), allocatable :: parent_id
    character(len=:), allocatable :: agent_id
    logical :: ephemeral = .false.
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: data_json
  end type session_event

  ! --------------------------------------------------------------------------
  ! Send options
  ! --------------------------------------------------------------------------
  type :: send_options
    character(len=:), allocatable :: message
    character(len=:), allocatable :: command
    character(len=:), allocatable :: model
  end type send_options

  ! --------------------------------------------------------------------------
  ! Send-and-wait result
  ! --------------------------------------------------------------------------
  type :: send_and_wait_result
    character(len=:), allocatable :: message
    character(len=:), allocatable :: error_message
    logical :: is_error = .false.
  end type send_and_wait_result

  ! --------------------------------------------------------------------------
  ! Tool invocation context
  ! --------------------------------------------------------------------------
  type :: tool_invocation
    character(len=:), allocatable :: invocation_id
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: arguments_json
  end type tool_invocation

  ! --------------------------------------------------------------------------
  ! Tool result
  ! --------------------------------------------------------------------------
  type :: tool_result
    character(len=:), allocatable :: content
    character(len=:), allocatable :: error_text
    logical :: is_error = .false.
  end type tool_result

  ! --------------------------------------------------------------------------
  ! Tool definition
  ! --------------------------------------------------------------------------
  type :: copilot_tool
    character(len=:), allocatable :: name
    character(len=:), allocatable :: description
    character(len=:), allocatable :: input_schema_json
    procedure(tool_callback_interface), pointer, nopass :: callback => null()
  end type copilot_tool

  ! --------------------------------------------------------------------------
  ! Permission request / result
  ! --------------------------------------------------------------------------
  type :: permission_request
    character(len=:), allocatable :: kind
    character(len=:), allocatable :: description
    character(len=:), allocatable :: resource
    character(len=:), allocatable :: session_id
  end type permission_request

  type :: permission_result
    logical :: granted = .false.
    character(len=:), allocatable :: reason
  end type permission_result

  ! --------------------------------------------------------------------------
  ! Elicitation request / result
  ! --------------------------------------------------------------------------
  type :: elicitation_request
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: message
    character(len=:), allocatable :: mode
    character(len=:), allocatable :: schema_json
  end type elicitation_request

  type :: elicitation_result
    character(len=:), allocatable :: action
    character(len=:), allocatable :: content_json
  end type elicitation_result

  ! --------------------------------------------------------------------------
  ! Abstract interfaces for callbacks
  ! --------------------------------------------------------------------------
  abstract interface
    function tool_callback_interface(args, invocation) result(res)
      import :: tool_invocation, tool_result
      character(len=*), intent(in) :: args
      type(tool_invocation), intent(in) :: invocation
      type(tool_result) :: res
    end function tool_callback_interface
  end interface

contains

  !> Set sensible defaults for client options.
  subroutine client_options_set_defaults(self)
    class(copilot_client_options), intent(inout) :: self

    if (.not. allocated(self%log_level)) then
      self%log_level = 'info'
    end if
    if (.not. allocated(self%cwd)) then
      self%cwd = '.'
    end if
    self%auto_start = .true.
    self%use_stdio  = .true.
  end subroutine client_options_set_defaults

end module copilot_types
