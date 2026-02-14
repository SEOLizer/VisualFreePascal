{$mode objfpc}{$H+}
unit dfm_parser;

interface

uses
  SysUtils, Classes, dfm_ast;

type
  TDfmParser = class
  public
    function ParseText(const AText: string): TDfmDocument;
  end;

implementation

function TDfmParser.ParseText(const AText: string): TDfmDocument;

  function ParseObject(StartPos: Integer; out EndPos: Integer): TDfmNode;
  var
    Lines: TStringArray;
    LineIdx: Integer;
    Line: string;
    Trimmed: string;
    InObject: Boolean;
    ObjectDepth: Integer;
    InstName, ClsName: string;
    PropName, PropValue: string;
    ColonPos: Integer;
    EqualsPos: Integer;
    ChildEndPos: Integer;
    ChildText: string;
    ChildStartIdx: Integer;
    ChildDepth: Integer;
    J, K: Integer;
    ChildParser: TDfmParser;
    ChildDoc: TDfmDocument;
    Val: TDfmValue;
    ChildLine: string;
  begin
    Result := nil;
    EndPos := StartPos;
    
    Lines := AText.Split([#13#10, #10]);
    LineIdx := 0;
    InObject := False;
    ObjectDepth := 0;
    
    while LineIdx < Length(Lines) do
    begin
      Line := Lines[LineIdx];
      Trimmed := Trim(Line);
      
      if Trimmed = '' then
      begin
        Inc(LineIdx);
        Continue;
      end;
      
      if SameText(Trimmed, 'end') then
      begin
        if InObject and (ObjectDepth = 1) then
        begin
          // Ende des aktuellen Objekts
          EndPos := StartPos + Length(Line) + 1;
          Inc(LineIdx);
          Break;
        end
        else if InObject then
        begin
          // Ende eines Child-Objekts
          Dec(ObjectDepth);
          Inc(LineIdx);
        end;
      end
      else if Pos('object ', LowerCase(Trimmed)) = 1 then
      begin
        if not InObject then
        begin
          // Start des Root-Objekts
          InObject := True;
          ObjectDepth := 1;
          
          // Parse: object InstanceName: ClassName
          Delete(Trimmed, 1, 7); // Remove "object "
          ColonPos := Pos(':', Trimmed);
          if ColonPos > 0 then
          begin
            InstName := Trim(Copy(Trimmed, 1, ColonPos - 1));
            ClsName := Trim(Copy(Trimmed, ColonPos + 1, Length(Trimmed)));
          end
          else
          begin
            InstName := Trim(Trimmed);
            ClsName := 'TObject';
          end;
          
          Result := TDfmNode.Create(InstName, ClsName);
          Inc(LineIdx);
        end
        else
        begin
          // Start eines Child-Objekts
          Inc(ObjectDepth);
          
          // Rekursiv parsen
          ChildEndPos := 0;
          ChildText := '';
          ChildStartIdx := LineIdx;
          
          // Finde das Ende des Child-Objekts
          ChildDepth := 1;
          J := LineIdx + 1;
          while J < Length(Lines) do
          begin
            ChildLine := Trim(Lines[J]);
            if Pos('object ', LowerCase(ChildLine)) = 1 then
              Inc(ChildDepth)
            else if SameText(ChildLine, 'end') then
            begin
              Dec(ChildDepth);
              if ChildDepth = 0 then
              begin
                // Child-Objekt endet hier
                for K := ChildStartIdx to J do
                begin
                  if ChildText <> '' then ChildText := ChildText + #10;
                  ChildText := ChildText + Lines[K];
                end;
                Break;
              end;
            end;
            Inc(J);
          end;
          
          if ChildText <> '' then
          begin
            ChildParser := TDfmParser.Create;
            try
              ChildDoc := ChildParser.ParseText(ChildText);
              if Assigned(ChildDoc.Root) then
              begin
                Result.AddChild(ChildDoc.Root);
                ChildDoc.Root := nil; // Ownership übertragen
              end;
              ChildDoc.Free;
            finally
              ChildParser.Free;
            end;
          end;
          
          LineIdx := J + 1;
        end;
      end
      else if InObject and (ObjectDepth = 1) and (Pos('=', Trimmed) > 0) then
      begin
        // Property
        EqualsPos := Pos('=', Trimmed);
        PropName := Trim(Copy(Trimmed, 1, EqualsPos - 1));
        PropValue := Trim(Copy(Trimmed, EqualsPos + 1, Length(Trimmed)));
        
        // Wert-Typ bestimmen
        if SameText(PropValue, 'True') then
          Val := TDfmValue.Create(dvkBoolean, 'True')
        else if SameText(PropValue, 'False') then
          Val := TDfmValue.Create(dvkBoolean, 'False')
        else if SameText(PropValue, 'nil') then
          Val := TDfmValue.Create(dvkNil, 'nil')
        else if (Length(PropValue) > 0) and (PropValue[1] in ['0'..'9', '-', '+']) then
        begin
          if Pos('.', PropValue) > 0 then
            Val := TDfmValue.Create(dvkFloat, PropValue)
          else
            Val := TDfmValue.Create(dvkInteger, PropValue);
        end
        else if (Length(PropValue) > 0) and (PropValue[1] = '''') then
          Val := TDfmValue.Create(dvkString, PropValue)
        else
          Val := TDfmValue.Create(dvkIdentifier, PropValue);
          
        Result.Properties.AddObject(PropName, Val);
        Inc(LineIdx);
      end
      else
      begin
        Inc(LineIdx);
      end;
    end;
  end;

var
  EndPos: Integer;
begin
  Result := TDfmDocument.Create;
  EndPos := 0;
  Result.Root := ParseObject(1, EndPos);
end;

end.
