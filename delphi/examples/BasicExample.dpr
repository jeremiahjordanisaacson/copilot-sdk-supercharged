{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

program BasicExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.JSON,
  Copilot.Client, Copilot.Session, Copilot.Types, Copilot.DefineTool;

var
  Client: TCopilotClient;
  Session: TCopilotSession;
  Config: TSessionConfig;
  MsgOpts: TMessageOptions;
  Response: TJSONValue;
  TimeTool: TTool;
begin
  try
    // Define a simple tool
    TimeTool := DefineTool(
      'get_current_time',
      'Returns the current date and time',
      nil,
      function(const Args: TJSONObject;
        const Inv: TToolInvocation): TToolResultObject
      begin
        Result := ToolSuccess(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      end
    );

    // Create and start client
    Client := TCopilotClient.Create;
    try
      Client.Start;
      WriteLn('Client started, protocol OK.');

      // Configure session
      Config := Default(TSessionConfig);
      Config.Streaming := True;
      Config.Tools := [TimeTool];
      Config.OnPermissionRequest :=
        function(const Req: TPermissionRequest): TPermissionResult
        begin
          WriteLn('Permission requested: ', Req.Kind, ' - ', Req.Description);
          Result.Decision := pdApproved;
        end;

      // Create session
      Session := Client.CreateSession(Config);
      WriteLn('Session created: ', Session.SessionId);

      // Subscribe to events
      Session.On_(
        procedure(const Event: TSessionEvent)
        begin
          if Event.EventType = 'assistant.message_delta' then
            Write(Event.Data.GetValue<string>('content', ''))
          else if Event.EventType = 'assistant.message' then
            WriteLn
          else if Event.EventType = 'session.idle' then
            WriteLn('[Session idle]');
        end
      );

      // Send a message and wait for the full response
      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'What is the current time? Use the get_current_time tool.';

      Response := Session.SendAndWait(MsgOpts);
      try
        if Assigned(Response) then
          WriteLn('Response: ', Response.ToJSON);
      finally
        Response.Free;
      end;

      // Clean up session
      Session.Disconnect;
      WriteLn('Session disconnected.');

    finally
      Client.Stop;
      Client.Free;
    end;

    WriteLn('Done.');
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
