{$mode objfpc}{$H+}
unit dfm_parser_debug;

interface

uses
  SysUtils, Classes, dfm_ast;

type
  TDfmParser = class
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    
    function Peek: Char;
    function Next: Char;
    procedure SkipWS;
  public
    constructor Create(const ASource: string);
    function Parse: TDfmDocument;
  end;

implementation

constructor TDfmParser.Create(const ASource: string);
begin
  FSource := ASource;
  FPos := 1;
  FLine := 1;
end;

function TDfmParser.Peek: Char;
begin
  if FPos <= Length(FSource) then
    Result := FSource[FPos]
  else
    Result := #0;
end;

function TDfmParser.Next: Char;
begin
  if FPos <= Length(FSource) then
  begin
    Result := FSource[FPos];
    Inc(FPos);
    if Result = #10 then Inc(FLine);
  end
  else
    Result := #0;
end;

procedure TDfmParser.SkipWS;
begin
  while Peek in [' ', #9, #13, #10] do Next;
end;

function TDfmParser.Parse: TDfmDocument;

  function ReadIdent: string;
  begin
    Result := '';
    while Peek in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
      Result := Result + Next;
  end;

  function ParseObject(Depth: Integer): TDfmNode;
  var
    Token, InstName, ClsName: string;
    Iteration: Integer;
  begin
    Result := nil;
    Iteration := 0;
    
    WriteLn('[', Depth, '] ParseObject start, Line=', FLine);
    
    // object
    SkipWS;
    Token := ReadIdent;
    WriteLn('[', Depth, '] After reading "object": ', Token);
    
    if not SameText(Token, 'object') then
    begin
      WriteLn('[', Depth, '] ERROR: Expected "object" but got "', Token, '"');
      Exit;
    end;
    
    // InstanceName
    SkipWS;
    InstName := ReadIdent;
    WriteLn('[', Depth, '] InstanceName: ', InstName);
    
    // :
    SkipWS;
    if Peek = ':' then
    begin
      Next; // consume :
      SkipWS;
      ClsName := ReadIdent;
      WriteLn('[', Depth, '] ClassName: ', ClsName);
    end
    else
    begin
      WriteLn('[', Depth, '] ERROR: Expected ":"');
      Exit;
    end;
    
    Result := TDfmNode.Create(InstName, ClsName);
    
    // Parse Properties und Kinder
    WriteLn('[', Depth, '] Entering property/child loop');
    while True do
    begin
      Inc(Iteration);
      if Iteration > 100 then
      begin
        WriteLn('[', Depth, '] ERROR: Too many iterations, possible infinite loop');
        Break;
      end;
      
      SkipWS;
      Token := ReadIdent;
      WriteLn('[', Depth, '] Loop iteration ', Iteration, ', Token="', Token, '"');
      
      if SameText(Token, 'end') then
      begin
        WriteLn('[', Depth, '] Found "end", breaking loop');
        Break;
      end
      else if SameText(Token, 'object') then
      begin
        WriteLn('[', Depth, '] Found child "object", recursing...');
        // Wir haben schon "object" gelesen, müssen aber zurücksetzen
        // Eigentlich müssten wir hier anders vorgehen
        Result.AddChild(ParseObject(Depth + 1));
      end
      else if Token <> '' then
      begin
        // Property
        WriteLn('[', Depth, '] Property: ', Token);
        SkipWS;
        if Peek = '=' then
        begin
          Next; // consume =
          SkipWS;
          // Lese Wert bis Zeilenende
          while not (Peek in [#13, #10, #0]) do Next;
        end;
      end
      else
      begin
        WriteLn('[', Depth, '] Empty token, breaking');
        Break;
      end;
    end;
    
    WriteLn('[', Depth, '] ParseObject end');
  end;

begin
  Result := TDfmDocument.Create;
  Result.Root := ParseObject(0);
end;

end.
