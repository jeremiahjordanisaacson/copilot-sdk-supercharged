! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Minimal JSON helpers used by the SDK wire layer.
!! Builds escaped JSON strings and splits JSON objects into members whose values are
!! kept as raw JSON text, so payloads can be inspected and regrouped without a DOM.
module copilot_json
  implicit none
  private

  public :: json_member, json_object
  public :: json_escape, json_quote, json_unescape, json_trim
  public :: json_parse_object, json_object_text
  public :: json_object_add, json_object_find, json_object_put
  public :: json_get_raw, json_get_path, json_get_string, json_get_logical, json_get_integer
  public :: json_is_null, json_is_string, json_is_object, json_string_array

  character(len=1), parameter :: BACKSLASH = achar(92)
  character(len=1), parameter :: QUOTE = achar(34)

  !> One object member: its name and its raw JSON value text.
  type :: json_member
    character(len=:), allocatable :: name
    character(len=:), allocatable :: value
    !> Set for members that came from an explicit (already grouped) object.
    logical :: explicit = .false.
  end type json_member

  !> An ordered list of object members.
  type :: json_object
    type(json_member), allocatable :: items(:)
    integer :: count = 0
  end type json_object

contains

  !> True for JSON insignificant whitespace.
  pure function is_ws(c) result(ws)
    character(len=1), intent(in) :: c
    logical :: ws

    ws = c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13)
  end function is_ws

  !> Strip leading and trailing JSON whitespace.
  function json_trim(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: first, last

    first = 1
    last = len(s)
    do while (first <= last)
      if (.not. is_ws(s(first:first))) exit
      first = first + 1
    end do
    do while (last >= first)
      if (.not. is_ws(s(last:last))) exit
      last = last - 1
    end do
    r = s(first:last)
  end function json_trim

  !> Escape text for use inside a JSON string literal.
  function json_escape(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    character(len=5) :: hex
    character(len=1) :: c
    integer :: i, code

    r = ''
    do i = 1, len(s)
      c = s(i:i)
      code = ichar(c)
      if (c == QUOTE) then
        r = r // BACKSLASH // QUOTE
      else if (c == BACKSLASH) then
        r = r // BACKSLASH // BACKSLASH
      else if (code == 10) then
        r = r // BACKSLASH // 'n'
      else if (code == 13) then
        r = r // BACKSLASH // 'r'
      else if (code == 9) then
        r = r // BACKSLASH // 't'
      else if (code == 8) then
        r = r // BACKSLASH // 'b'
      else if (code == 12) then
        r = r // BACKSLASH // 'f'
      else if (code >= 0 .and. code < 32) then
        write(hex, '(A,Z4.4)') 'u', code
        r = r // BACKSLASH // hex
      else
        r = r // c
      end if
    end do
  end function json_escape

  !> Quote and escape text as a JSON string literal.
  function json_quote(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r

    r = QUOTE // json_escape(s) // QUOTE
  end function json_quote

  !> Decode the body of a JSON string literal (the text between the quotes).
  function json_unescape(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    character(len=1) :: c
    integer :: i, n, code, ios

    r = ''
    n = len(s)
    i = 1
    do while (i <= n)
      c = s(i:i)
      if (c /= BACKSLASH .or. i == n) then
        r = r // c
        i = i + 1
        cycle
      end if
      c = s(i + 1:i + 1)
      select case (c)
      case ('n')
        r = r // achar(10)
      case ('r')
        r = r // achar(13)
      case ('t')
        r = r // achar(9)
      case ('b')
        r = r // achar(8)
      case ('f')
        r = r // achar(12)
      case ('u')
        code = -1
        if (i + 5 <= n) then
          read(s(i + 2:i + 5), '(Z4)', iostat=ios) code
          if (ios /= 0) code = -1
        end if
        if (code >= 0) then
          r = r // utf8_encode(code)
          i = i + 6
          cycle
        end if
        r = r // c
      case default
        r = r // c
      end select
      i = i + 2
    end do
  end function json_unescape

  !> Encode a Basic Multilingual Plane code point as UTF-8.
  function utf8_encode(code) result(r)
    integer, intent(in) :: code
    character(len=:), allocatable :: r

    if (code < 128) then
      r = achar(code)
    else if (code < 2048) then
      r = char(192 + code / 64) // char(128 + mod(code, 64))
    else
      r = char(224 + code / 4096) // char(128 + mod(code / 64, 64)) // char(128 + mod(code, 64))
    end if
  end function utf8_encode

  !> True when the raw value is the JSON literal null.
  function json_is_null(raw) result(is_null)
    character(len=*), intent(in) :: raw
    logical :: is_null

    is_null = json_trim(raw) == 'null'
  end function json_is_null

  !> True when the raw value is a JSON string literal.
  function json_is_string(raw) result(is_string)
    character(len=*), intent(in) :: raw
    logical :: is_string
    character(len=:), allocatable :: t

    t = json_trim(raw)
    is_string = .false.
    if (len(t) < 2) return
    is_string = t(1:1) == QUOTE .and. t(len(t):len(t)) == QUOTE
  end function json_is_string

  !> True when the raw value is a JSON object.
  function json_is_object(raw) result(is_object)
    character(len=*), intent(in) :: raw
    logical :: is_object
    character(len=:), allocatable :: t

    t = json_trim(raw)
    is_object = .false.
    if (len(t) < 2) return
    is_object = t(1:1) == '{' .and. t(len(t):len(t)) == '}'
  end function json_is_object

  !> Append a member (duplicates allowed).
  subroutine json_object_add(obj, name, value, explicit)
    type(json_object), intent(inout) :: obj
    character(len=*), intent(in) :: name, value
    logical, intent(in), optional :: explicit
    type(json_member), allocatable :: grown(:)
    integer :: i

    if (.not. allocated(obj%items)) then
      allocate(obj%items(16))
      obj%count = 0
    else if (obj%count >= size(obj%items)) then
      allocate(grown(2 * size(obj%items)))
      do i = 1, obj%count
        grown(i) = obj%items(i)
      end do
      call move_alloc(grown, obj%items)
    end if
    obj%count = obj%count + 1
    obj%items(obj%count)%name = name
    obj%items(obj%count)%value = value
    obj%items(obj%count)%explicit = .false.
    if (present(explicit)) obj%items(obj%count)%explicit = explicit
  end subroutine json_object_add

  !> Index of the first member with the given name, or 0.
  function json_object_find(obj, name) result(idx)
    type(json_object), intent(in) :: obj
    character(len=*), intent(in) :: name
    integer :: idx
    integer :: i

    idx = 0
    do i = 1, obj%count
      if (obj%items(i)%name == name .and. len(obj%items(i)%name) == len(name)) then
        idx = i
        return
      end if
    end do
  end function json_object_find

  !> Set a member unless a stronger value is already present.
  !! An explicit value replaces an earlier non-explicit one; otherwise the first value wins.
  subroutine json_object_put(obj, name, value, explicit)
    type(json_object), intent(inout) :: obj
    character(len=*), intent(in) :: name, value
    logical, intent(in) :: explicit
    integer :: idx

    idx = json_object_find(obj, name)
    if (idx == 0) then
      call json_object_add(obj, name, value, explicit)
    else if (explicit .and. .not. obj%items(idx)%explicit) then
      obj%items(idx)%value = value
      obj%items(idx)%explicit = .true.
    end if
  end subroutine json_object_put

  !> Serialize members back to JSON object text.
  function json_object_text(obj) result(text)
    type(json_object), intent(in) :: obj
    character(len=:), allocatable :: text
    integer :: i

    text = '{'
    do i = 1, obj%count
      if (i > 1) text = text // ','
      text = text // json_quote(obj%items(i)%name) // ':' // obj%items(i)%value
    end do
    text = text // '}'
  end function json_object_text

  !> Split the top-level members of a JSON object into names and raw values.
  !! ok is .false. when the text is not a JSON object.
  subroutine json_parse_object(json, obj, ok)
    character(len=*), intent(in) :: json
    type(json_object), intent(out) :: obj
    logical, intent(out), optional :: ok
    character(len=1) :: c
    integer :: n, i, first, depth, start, colon
    logical :: in_string, escaped

    if (present(ok)) ok = .false.
    n = len(json)
    first = 1
    do while (first <= n)
      if (.not. is_ws(json(first:first))) exit
      first = first + 1
    end do
    if (first > n) return
    if (json(first:first) /= '{') return
    if (present(ok)) ok = .true.

    depth = 0
    in_string = .false.
    escaped = .false.
    start = first + 1
    colon = 0
    do i = first, n
      c = json(i:i)
      if (in_string) then
        if (escaped) then
          escaped = .false.
        else if (c == BACKSLASH) then
          escaped = .true.
        else if (c == QUOTE) then
          in_string = .false.
        end if
        cycle
      end if
      select case (c)
      case ('"')
        in_string = .true.
      case ('{', '[')
        depth = depth + 1
      case ('}', ']')
        depth = depth - 1
        if (depth == 0) then
          call add_raw_member(json, start, colon, i - 1, obj)
          exit
        end if
      case (':')
        if (depth == 1 .and. colon == 0) colon = i
      case (',')
        if (depth == 1) then
          call add_raw_member(json, start, colon, i - 1, obj)
          start = i + 1
          colon = 0
        end if
      end select
    end do
  end subroutine json_parse_object

  !> Add the member spanning json(start:finish) with its name/value separator at colon.
  subroutine add_raw_member(json, start, colon, finish, obj)
    character(len=*), intent(in) :: json
    integer, intent(in) :: start, colon, finish
    type(json_object), intent(inout) :: obj
    character(len=:), allocatable :: key

    if (colon == 0) return
    key = json_trim(json(start:colon - 1))
    if (.not. json_is_string(key)) return
    call json_object_add(obj, json_unescape(key(2:len(key) - 1)), json_trim(json(colon + 1:finish)))
  end subroutine add_raw_member

  !> Raw JSON text of a top-level member of an object.
  subroutine json_get_raw(json, name, value, found)
    character(len=*), intent(in) :: json, name
    character(len=:), allocatable, intent(out) :: value
    logical, intent(out) :: found
    type(json_object) :: obj
    integer :: idx

    value = ''
    found = .false.
    call json_parse_object(json, obj)
    idx = json_object_find(obj, name)
    if (idx == 0) return
    value = obj%items(idx)%value
    found = .true.
  end subroutine json_get_raw

  !> Raw JSON text at a dotted member path, for example 'params.event.data'.
  subroutine json_get_path(json, path, value, found)
    character(len=*), intent(in) :: json, path
    character(len=:), allocatable, intent(out) :: value
    logical, intent(out) :: found
    character(len=:), allocatable :: current, rest, next, tail
    integer :: dot

    current = json
    rest = path
    found = .false.
    value = ''
    do
      dot = index(rest, '.')
      if (dot == 0) then
        call json_get_raw(current, rest, value, found)
        return
      end if
      call json_get_raw(current, rest(1:dot - 1), next, found)
      if (.not. found) return
      current = next
      tail = rest(dot + 1:)
      rest = tail
    end do
  end subroutine json_get_path

  !> Decoded string value of a top-level member ('' when absent or not a string).
  function json_get_string(json, name, found) result(value)
    character(len=*), intent(in) :: json, name
    logical, intent(out), optional :: found
    character(len=:), allocatable :: value
    character(len=:), allocatable :: raw
    logical :: is_set

    value = ''
    call json_get_raw(json, name, raw, is_set)
    if (is_set) is_set = json_is_string(raw)
    if (is_set) value = json_unescape(raw(2:len(raw) - 1))
    if (present(found)) found = is_set
  end function json_get_string

  !> Boolean value of a top-level member, or default when absent or not a boolean.
  function json_get_logical(json, name, default) result(value)
    character(len=*), intent(in) :: json, name
    logical, intent(in) :: default
    logical :: value
    character(len=:), allocatable :: raw
    logical :: is_set

    value = default
    call json_get_raw(json, name, raw, is_set)
    if (.not. is_set) return
    if (raw == 'true') then
      value = .true.
    else if (raw == 'false') then
      value = .false.
    end if
  end function json_get_logical

  !> Integer value of a top-level member.
  subroutine json_get_integer(json, name, value, found)
    character(len=*), intent(in) :: json, name
    integer, intent(out) :: value
    logical, intent(out) :: found
    character(len=:), allocatable :: raw
    integer :: ios

    value = 0
    call json_get_raw(json, name, raw, found)
    if (.not. found) return
    if (len(raw) == 0 .or. verify(raw, '-0123456789') /= 0) then
      found = .false.
      return
    end if
    read(raw, *, iostat=ios) value
    if (ios /= 0) then
      value = 0
      found = .false.
    end if
  end subroutine json_get_integer

  !> JSON array of strings from a character array (each item is trimmed).
  function json_string_array(items) result(r)
    character(len=*), intent(in) :: items(:)
    character(len=:), allocatable :: r
    integer :: i

    r = '['
    do i = 1, size(items)
      if (i > 1) r = r // ','
      r = r // json_quote(trim(items(i)))
    end do
    r = r // ']'
  end function json_string_array

end module copilot_json
