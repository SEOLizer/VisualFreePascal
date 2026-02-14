{$mode objfpc}{$H+}
program dfm_features_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

procedure TestStrings;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
  J: Integer;
begin
  WriteLn('=== Test: String Escaping ===');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Caption = ''Hello World'''#13#10 +
           '  Hint = ''It''s working!'''#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    Doc := Parser.ParseText(Input);
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn('Parsed Properties:');
      for J := 0 to Doc.Root.Properties.Count - 1 do
        WriteLn('  ', Doc.Root.Properties[J], ' = ', 
                TDfmValue(Doc.Root.Properties.Objects[J]).AsString);
      
      Output := Writer.WriteDocument(Doc);
      WriteLn('Output:');
      WriteLn(Output);
      Doc.Free;
    end;
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

procedure TestSets;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
begin
  WriteLn('=== Test: Sets ===');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Anchors = [akLeft, akTop, akRight]'#13#10 +
           '  BorderStyle = bsDialog'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    Doc := Parser.ParseText(Input);
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      Output := Writer.WriteDocument(Doc);
      WriteLn('Output:');
      WriteLn(Output);
      Doc.Free;
    end;
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

procedure TestCollections;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
begin
  WriteLn('=== Test: Collections ===');
  WriteLn;
  
  Input := 'object ListBox1: TListBox'#13#10 +
           '  Items.Strings = <'#13#10 +
           '    item'#13#10 +
           '      Text = ''First'''#13#10 +
           '    end'#13#10 +
           '    item'#13#10 +
           '      Text = ''Second'''#13#10 +
           '    end>'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    Doc := Parser.ParseText(Input);
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      Output := Writer.WriteDocument(Doc);
      WriteLn('Output:');
      WriteLn(Output);
      Doc.Free;
    end;
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

begin
  WriteLn('DFM Features Test');
  WriteLn('=================');
  WriteLn;
  
  try
    TestStrings;
    WriteLn;
    TestSets;
    WriteLn;
    TestCollections;
    WriteLn;
    WriteLn('All feature tests completed!');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
