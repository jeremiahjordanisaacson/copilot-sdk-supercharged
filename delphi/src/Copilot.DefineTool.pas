{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.DefineTool;

interface

uses
  System.SysUtils, System.JSON,
  Copilot.Types;

type
  TDefineTool = record
    class function Build(const AName, ADescription: string;
      AParameters: TJSONObject; AHandler: TToolHandler): TTool; static;
    class function BuildSimple(const AName, ADescription: string;
      AHandler: TToolHandler): TTool; static;
  end;

  // Convenience function matching the pattern used by other SDKs
  function DefineTool(const AName, ADescription: string;
    AParameters: TJSONObject; AHandler: TToolHandler): TTool;

  function DefineSimpleTool(const AName, ADescription: string;
    AHandler: TToolHandler): TTool;

implementation

function DefineTool(const AName, ADescription: string;
  AParameters: TJSONObject; AHandler: TToolHandler): TTool;
begin
  Result := TDefineTool.Build(AName, ADescription, AParameters, AHandler);
end;

function DefineSimpleTool(const AName, ADescription: string;
  AHandler: TToolHandler): TTool;
begin
  Result := TDefineTool.BuildSimple(AName, ADescription, AHandler);
end;

{ TDefineTool }

class function TDefineTool.Build(const AName, ADescription: string;
  AParameters: TJSONObject; AHandler: TToolHandler): TTool;
begin
  Result.Name := AName;
  Result.Description := ADescription;
  Result.Parameters := AParameters;
  Result.Handler := AHandler;
end;

class function TDefineTool.BuildSimple(const AName, ADescription: string;
  AHandler: TToolHandler): TTool;
begin
  Result.Name := AName;
  Result.Description := ADescription;
  Result.Parameters := nil;
  Result.Handler := AHandler;
end;

end.
