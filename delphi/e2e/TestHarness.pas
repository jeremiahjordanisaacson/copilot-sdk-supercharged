{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit TestHarness;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, RegExpr, fphttpclient, fpjson, jsonparser;

type
  { TCapiProxy manages a replaying CAPI proxy for E2E tests.
    It spawns the shared test harness server from test/harness/server.ts. }
  TCapiProxy = class
  private
    FProcess: TProcess;
    FProxyUrl: string;
    FRepoRoot: string;
    function ComputeRepoRoot: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Start: string;
    procedure Stop;
    procedure Configure(const FilePath, WorkDir: string);
    property ProxyUrl: string read FProxyUrl;
    property RepoRoot: string read FRepoRoot;
  end;

implementation

{ TCapiProxy }

constructor TCapiProxy.Create;
begin
  inherited Create;
  FProcess := nil;
  FProxyUrl := '';
  FRepoRoot := ComputeRepoRoot;
end;

destructor TCapiProxy.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TCapiProxy.ComputeRepoRoot: string;
var
  Dir: string;
begin
  // This file lives at <repo>/delphi/e2e/TestHarness.pas
  // From the e2e directory go up 3 levels: e2e -> delphi -> repo root
  // At runtime use the executable's location as the anchor
  Dir := ExtractFilePath(ParamStr(0));
  if Dir = '' then
    Dir := GetCurrentDir;

  // Walk up from delphi/e2e (or wherever the binary is) to repo root.
  // Try to find test/harness/server.ts by walking up directories.
  while Dir <> '' do
  begin
    if FileExists(IncludeTrailingPathDelimiter(Dir) + 'test' +
      DirectorySeparator + 'harness' + DirectorySeparator + 'server.ts') then
    begin
      Result := Dir;
      Exit;
    end;
    // Move up one directory
    if Dir = ExtractFileDir(Dir) then
      Break;  // Reached filesystem root
    Dir := ExtractFileDir(Dir);
  end;

  // Fallback: assume CWD is within the repo somewhere, walk up from there
  Dir := GetCurrentDir;
  while Dir <> '' do
  begin
    if FileExists(IncludeTrailingPathDelimiter(Dir) + 'test' +
      DirectorySeparator + 'harness' + DirectorySeparator + 'server.ts') then
    begin
      Result := Dir;
      Exit;
    end;
    if Dir = ExtractFileDir(Dir) then
      Break;
    Dir := ExtractFileDir(Dir);
  end;

  // Last resort: assume we're 3 levels deep (delphi/e2e/binary)
  Result := ExpandFileName(
    IncludeTrailingPathDelimiter(GetCurrentDir) + '..' +
    DirectorySeparator + '..' + DirectorySeparator + '..');
end;

function TCapiProxy.Start: string;
var
  ServerPath: string;
  HarnessDir: string;
  Line: string;
  RE: TRegExpr;
begin
  if FProxyUrl <> '' then
  begin
    Result := FProxyUrl;
    Exit;
  end;

  ServerPath := IncludeTrailingPathDelimiter(FRepoRoot) +
    'test' + DirectorySeparator + 'harness' + DirectorySeparator + 'server.ts';
  HarnessDir := IncludeTrailingPathDelimiter(FRepoRoot) +
    'test' + DirectorySeparator + 'harness';

  if not FileExists(ServerPath) then
    raise Exception.CreateFmt('Harness server not found at: %s', [ServerPath]);

  FProcess := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    // On Windows, use cmd /c to find npx on PATH
    FProcess.Executable := 'cmd';
    FProcess.Parameters.Add('/c');
    FProcess.Parameters.Add('npx');
    {$ELSE}
    FProcess.Executable := 'npx';
    {$ENDIF}
    FProcess.Parameters.Add('tsx');
    FProcess.Parameters.Add(ServerPath);
    FProcess.CurrentDirectory := HarnessDir;
    FProcess.Options := [poUsePipes, poStderrToOutPut];

    FProcess.Execute;

    // Read stdout line-by-line until we get the Listening URL
    Line := '';
    RE := TRegExpr.Create('Listening:\s+(http://[^\s]+)');
    try
      while FProcess.Running do
      begin
        if FProcess.Output.NumBytesAvailable > 0 then
        begin
          SetLength(Line, FProcess.Output.NumBytesAvailable);
          FProcess.Output.Read(Line[1], Length(Line));

          if RE.Exec(Line) then
          begin
            FProxyUrl := RE.Match[1];
            Break;
          end;
        end
        else
          Sleep(50);
      end;
    finally
      RE.Free;
    end;

    if FProxyUrl = '' then
    begin
      FProcess.Terminate(1);
      FreeAndNil(FProcess);
      raise Exception.Create('Failed to read proxy URL from harness output');
    end;

    // Set env var so CLI can find the proxy
    SetEnvironmentVariable('COPILOT_API_URL', PChar(FProxyUrl));

    Result := FProxyUrl;
  except
    FreeAndNil(FProcess);
    raise;
  end;
end;

procedure TCapiProxy.Stop;
var
  Http: TFPHTTPClient;
  StopUrl: string;
begin
  if (FProcess = nil) or (FProxyUrl = '') then
    Exit;

  // Send POST /stop to gracefully shut down the proxy
  Http := TFPHTTPClient.Create(nil);
  try
    try
      StopUrl := FProxyUrl + '/stop';
      Http.AddHeader('Content-Type', 'application/json');
      Http.RequestBody := TStringStream.Create('{}');
      try
        Http.Post(StopUrl);
      finally
        Http.RequestBody.Free;
        Http.RequestBody := nil;
      end;
    except
      // Best effort — proxy may already be gone
    end;
  finally
    Http.Free;
  end;

  // Wait for process to exit (with timeout)
  if FProcess.Running then
  begin
    FProcess.WaitOnExit(5000);
    if FProcess.Running then
      FProcess.Terminate(1);
  end;

  FreeAndNil(FProcess);
  FProxyUrl := '';
end;

procedure TCapiProxy.Configure(const FilePath, WorkDir: string);
var
  Http: TFPHTTPClient;
  ConfigUrl: string;
  Body: TJSONObject;
  BodyStr: string;
  Response: string;
begin
  if FProxyUrl = '' then
    raise Exception.Create('Proxy not started');

  ConfigUrl := FProxyUrl + '/config';

  Body := TJSONObject.Create;
  try
    Body.Add('filePath', FilePath);
    Body.Add('workDir', WorkDir);
    BodyStr := Body.AsJSON;
  finally
    Body.Free;
  end;

  Http := TFPHTTPClient.Create(nil);
  try
    Http.AddHeader('Content-Type', 'application/json');
    Http.RequestBody := TStringStream.Create(BodyStr);
    try
      Response := Http.Post(ConfigUrl);
    finally
      Http.RequestBody.Free;
      Http.RequestBody := nil;
    end;

    if Http.ResponseStatusCode <> 200 then
      raise Exception.CreateFmt('Proxy config failed with status %d',
        [Http.ResponseStatusCode]);
  finally
    Http.Free;
  end;
end;

end.
