{$mode objfpc}{$H+}
program dfm_simple_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser;

var
  Parser: TDfmParser;
  Doc: TDfmDocument;
  Input: string;
  I: Integer;
begin
  WriteLn('Simple DFM Test');
  WriteLn('===============');
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 100'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    WriteLn('Parsing...');
    Doc := Parser.Parse;
    
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn('✓ Parse successful!');
      WriteLn('  Name: ', Doc.Root.InstanceName);
      WriteLn('  Class: ', Doc.Root.ClassName);
      WriteLn('  Properties: ', Doc.Root.Properties.Count);
    end
    else
    begin
      WriteLn('✗ Parse failed');
      if Parser.Errors.Count > 0 then
      begin
        WriteLn('Errors:');
        for I := 0 to Parser.Errors.Count - 1 do
          WriteLn('  ', Parser.Errors[I]);
      end;
    end;
    
    if Assigned(Doc) then
      Doc.Free;
  finally
    Parser.Free;
  end;
  
  WriteLn('Done.');
end.
