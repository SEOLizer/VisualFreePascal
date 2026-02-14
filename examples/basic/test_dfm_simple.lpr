{$mode objfpc}{$H+}
program test_dfm_simple;

{==============================================================================}
{ Einfacher DFM Parser Test ohne Mini-LCL Abhängigkeiten                     }
{==============================================================================}

uses
  SysUtils, Classes;

// Forward declarations für Test
procedure RunTests;

// Einfacher DFM-Parser als Proof of Concept
type
  TDfmValueKind = (dvkInteger, dvkFloat, dvkString, dvkBoolean, dvkNil, dvkIdentifier, dvkSet, dvkCollection);

  TDfmValue = record
    Kind: TDfmValueKind;
    StrValue: string;
    IntValue: Int64;
    FloatValue: Double;
    BoolValue: Boolean;
  end;

  TDfmNode = class
    Name: string;
    ClassName: string;
    Properties: TStringList;
    Children: TList;
    constructor Create(const AName, AClassName: string);
    destructor Destroy; override;
    procedure AddChild(Child: TDfmNode);
    function FindChild(const AName: string): TDfmNode;
  end;

  TDfmParser = class
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FCol: Integer;
    FErrors: TStringList;
    
    function Peek: Char;
    function Next: Char;
    procedure SkipWS;
    function ParseIdent: string;
    function ParseString: string;
    function ParseNumber: string;
  public
    constructor Create(const ASource: string);
    destructor Destroy; override;
    function Parse: TDfmNode;
    property Errors: TStringList read FErrors;
  end;

{ TDfmNode }

constructor TDfmNode.Create(const AName, AClassName: string);
begin
  Name := AName;
  ClassName := AClassName;
  Properties := TStringList.Create;
  Children := TList.Create;
end;

destructor TDfmNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to Children.Count - 1 do
    TDfmNode(Children[I]).Free;
  Children.Free;
  Properties.Free;
  inherited;
end;

procedure TDfmNode.AddChild(Child: TDfmNode);
begin
  Children.Add(Child);
end;

function TDfmNode.FindChild(const AName: string): TDfmNode;
var
  I: Integer;
begin
  for I := 0 to Children.Count - 1 do
  begin
    Result := TDfmNode(Children[I]);
    if SameText(Result.Name, AName) then Exit;
  end;
  Result := nil;
end;

{ TDfmParser }

constructor TDfmParser.Create(const ASource: string);
begin
  FSource := ASource;
  FPos := 1;
  FLine := 1;
  FCol := 1;
  FErrors := TStringList.Create;
end;

destructor TDfmParser.Destroy;
begin
  FErrors.Free;
  inherited;
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
    Inc(FCol);
    if Result = #10 then
    begin
      Inc(FLine);
      FCol := 1;
    end;
  end
  else
    Result := #0;
end;

procedure TDfmParser.SkipWS;
begin
  while Peek in [' ', #9, #13, #10] do
    Next;
end;

function TDfmParser.ParseIdent: string;
begin
  Result := '';
  while Peek in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
    Result := Result + Next;
end;

function TDfmParser.ParseString: string;
var
  Ch: Char;
  InQuote: Boolean;
begin
  Result := '';
  InQuote := False;
  
  if Peek = '''' then
  begin
    Next; // consume '
    InQuote := True;
    
    while True do
    begin
      Ch := Peek;
      if Ch = #0 then Break;
      
      if Ch = '''' then
      begin
        Next;
        if Peek = '''' then
        begin
          Result := Result + Next; // escaped ''
        end
        else
        begin
          InQuote := False;
          Break;
        end;
      end
      else
        Result := Result + Next;
    end;
  end;
end;

function TDfmParser.ParseNumber: string;
begin
  Result := '';
  while Peek in ['0'..'9', '.', 'e', 'E', '-', '+'] do
    Result := Result + Next;
end;

function TDfmParser.Parse: TDfmNode;

  function ParseObject: TDfmNode;
  var
    Token: string;
    ObjName, ObjClass: string;
    PropName, PropValue: string;
  begin
    Result := nil;
    
    // Erwarte 'object'
    SkipWS;
    Token := ParseIdent;
    if not SameText(Token, 'object') then
    begin
      FErrors.Add(Format('[%d:%d] Expected "object" but found "%s"', [FLine, FCol, Token]));
      Exit;
    end;
    
    // Lese Name
    SkipWS;
    ObjName := ParseIdent;
    if ObjName = '' then
    begin
      FErrors.Add(Format('[%d:%d] Expected object name', [FLine, FCol]));
      Exit;
    end;
    
    // Lese ':'
    SkipWS;
    if Peek = ':' then
    begin
      Next;
      SkipWS;
      ObjClass := ParseIdent;
    end
    else
    begin
      FErrors.Add(Format('[%d:%d] Expected ":"', [FLine, FCol]));
      Exit;
    end;
    
    Result := TDfmNode.Create(ObjName, ObjClass);
    
    // Parse Properties und Kinder
    while True do
    begin
      SkipWS;
      
      // Prüfe auf 'end'
      if SameText(ParseIdent, 'end') then
        Break;
      
      // Zurücksetzen für Property-Parsing
      // (vereinfacht für diesen PoC)
      
      if Peek = #0 then
      begin
        FErrors.Add(Format('[%d:%d] Unexpected EOF', [FLine, FCol]));
        Break;
      end;
      
      // Überspringe für PoC
      Next;
    end;
  end;

begin
  Result := ParseObject;
end;

{ Tests }

procedure Test1;
var
  Parser: TDfmParser;
  Root: TDfmNode;
  Input: string;
begin
  WriteLn('=== Test 1: Basic Object Parsing ===');
  
  Input := 'object Form1: TForm'#13#10 +
           'end';
  
  WriteLn('Input: ', Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    Root := Parser.Parse;
    if Assigned(Root) then
    begin
      WriteLn('✓ Parsed successfully');
      WriteLn('  Name: ', Root.Name);
      WriteLn('  Class: ', Root.ClassName);
      Root.Free;
    end
    else
    begin
      WriteLn('✗ Parse failed');
    end;
    
    if Parser.Errors.Count > 0 then
    begin
      WriteLn('Errors:');
      for var I := 0 to Parser.Errors.Count - 1 do
        WriteLn('  ', Parser.Errors[I]);
    end;
  finally
    Parser.Free;
  end;
end;

procedure RunTests;
begin
  WriteLn('DFM Parser Proof of Concept');
  WriteLn('===========================');
  WriteLn;
  
  Test1;
  
  WriteLn;
  WriteLn('Tests completed.');
end;

begin
  RunTests;
end.
