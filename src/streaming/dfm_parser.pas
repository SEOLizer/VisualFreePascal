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
    OpenBracketPos, CloseBracketPos: Integer;
    OpenAnglePos, CloseAnglePos: Integer;
    SetContent: string;
    CollContent: string;
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
          EndPos := StartPos + Length(Line) + 1;
          Inc(LineIdx);
          Break;
        end
        else if InObject then
        begin
          Dec(ObjectDepth);
          Inc(LineIdx);
        end;
      end
      else if Pos('object ', LowerCase(Trimmed)) = 1 then
      begin
        if not InObject then
        begin
          InObject := True;
          ObjectDepth := 1;
          
          Delete(Trimmed, 1, 7);
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
          Inc(ObjectDepth);
          ChildEndPos := 0;
          ChildText := '';
          ChildStartIdx := LineIdx;
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
                ChildDoc.Root := nil;
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
        EqualsPos := Pos('=', Trimmed);
        PropName := Trim(Copy(Trimmed, 1, EqualsPos - 1));
        PropValue := Trim(Copy(Trimmed, EqualsPos + 1, Length(Trimmed)));
        
        // Prüfe auf Set: [akLeft, akTop]
        if (Length(PropValue) > 0) and (PropValue[1] = '[') then
        begin
          OpenBracketPos := Pos('[', PropValue);
          CloseBracketPos := Pos(']', PropValue);
          if CloseBracketPos = 0 then
          begin
            // Set erstreckt sich über mehrere Zeilen
            SetContent := PropValue;
            Inc(LineIdx);
            while LineIdx < Length(Lines) do
            begin
              SetContent := SetContent + ' ' + Trim(Lines[LineIdx]);
              if Pos(']', Trim(Lines[LineIdx])) > 0 then
                Break;
              Inc(LineIdx);
            end;
            Val := TDfmValue.Create(dvkSet, SetContent);
          end
          else
          begin
            Val := TDfmValue.Create(dvkSet, PropValue);
          end;
        end
        // Prüfe auf Collection: < item ... end >
        else if (Length(PropValue) > 0) and (PropValue[1] = '<') then
        begin
          OpenAnglePos := Pos('<', PropValue);
          CloseAnglePos := Pos('>', PropValue);
          if CloseAnglePos = 0 then
          begin
            // Collection erstreckt sich über mehrere Zeilen
            CollContent := PropValue;
            Inc(LineIdx);
            while LineIdx < Length(Lines) do
            begin
              CollContent := CollContent + ' ' + Trim(Lines[LineIdx]);
              if Pos('>', Trim(Lines[LineIdx])) > 0 then
                Break;
              Inc(LineIdx);
            end;
            Val := TDfmValue.Create(dvkCollection, CollContent);
          end
          else
          begin
            Val := TDfmValue.Create(dvkCollection, PropValue);
          end;
        end
        // Prüfe auf Binary: { ... }
        else if (Length(PropValue) > 0) and (PropValue[1] = '{') then
        begin
          Val := TDfmValue.Create(dvkBinary, PropValue);
        end
        else if SameText(PropValue, 'True') then
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
        begin
          // String: Entferne umschließende Quotes
          if (Length(PropValue) >= 2) and (PropValue[Length(PropValue)] = '''') then
            PropValue := Copy(PropValue, 2, Length(PropValue) - 2);
          Val := TDfmValue.Create(dvkString, PropValue);
        end
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
