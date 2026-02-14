{$mode objfpc}{$H+}
program dfm_full_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

procedure TestBasic;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
  ErrIdx: Integer;
begin
  WriteLn('=== Test 1: Basic Parsing & Writing ===');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 100'#13#10 +
           '  Top = 200'#13#10 +
           '  Caption = ''Test Form'''#13#10 +
           '  Visible = True'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  // Parse
  Parser := TDfmParser.Create(Input);
  try
    Doc := Parser.Parse;
    try
      WriteLn('Parsed successfully!');
      WriteLn('  Root: ', Doc.Root.InstanceName, ': ', Doc.Root.ClassName);
      WriteLn('  Properties: ', Doc.Root.Properties.Count);
      
      if Parser.Errors.Count > 0 then
      begin
        WriteLn('  Errors:');
        for ErrIdx := 0 to Parser.Errors.Count - 1 do
          WriteLn('    ', Parser.Errors[ErrIdx]);
      end;
      
      // Write back
      Writer := TDfmWriter.Create;
      try
        Output := Writer.WriteDocument(Doc);
        WriteLn('Output:');
        WriteLn(Output);
      finally
        Writer.Free;
      end;
      
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestRoundtrip;
var
  Parser1, Parser2: TDfmParser;
  Writer: TDfmWriter;
  Doc1, Doc2: TDfmDocument;
  Input, Middle, Output: string;
begin
  WriteLn('=== Test 2: Roundtrip ===');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  Top = 0'#13#10 +
           '  Width = 640'#13#10 +
           '  Height = 480'#13#10 +
           '  Caption = ''My Application'''#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '    Top = 10'#13#10 +
           '    Width = 75'#13#10 +
           '    Height = 25'#13#10 +
           '    Caption = ''OK'''#13#10 +
           '  end'#13#10 +
           'end';
  
  WriteLn('Original:');
  WriteLn(Input);
  WriteLn;
  
  // First parse
  Parser1 := TDfmParser.Create(Input);
  Writer := TDfmWriter.Create;
  try
    Doc1 := Parser1.Parse;
    try
      Middle := Writer.WriteDocument(Doc1);
      WriteLn('After first write:');
      WriteLn(Middle);
      WriteLn;
      
      // Second parse
      Parser2 := TDfmParser.Create(Middle);
      try
        Doc2 := Parser2.Parse;
        try
          Output := Writer.WriteDocument(Doc2);
          WriteLn('After second write:');
          WriteLn(Output);
          WriteLn;
          
          if Middle = Output then
            WriteLn('✓ Roundtrip successful!')
          else
            WriteLn('✗ Roundtrip failed - outputs differ');
            
        finally
          Doc2.Free;
        end;
      finally
        Parser2.Free;
      end;
      
    finally
      Doc1.Free;
    end;
  finally
    Parser1.Free;
    Writer.Free;
  end;
end;

procedure TestManualCreation;
var
  Doc: TDfmDocument;
  Form, Button: TDfmNode;
  Writer: TDfmWriter;
  Output: string;
begin
  WriteLn('=== Test 3: Manual AST Creation ===');
  WriteLn;
  
  // Erstelle Dokument manuell
  Doc := TDfmDocument.Create;
  try
    Form := TDfmNode.Create('MainForm', 'TForm');
    Doc.Root := Form;
    
    // Füge Properties hinzu
    Form.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '100'));
    Form.Properties.AddObject('Top', TDfmValue.Create(dvkInteger, '50'));
    Form.Properties.AddObject('Width', TDfmValue.Create(dvkInteger, '800'));
    Form.Properties.AddObject('Height', TDfmValue.Create(dvkInteger, '600'));
    Form.Properties.AddObject('Caption', TDfmValue.Create(dvkString, 'My App'));
    Form.Properties.AddObject('Visible', TDfmValue.Create(dvkBoolean, 'True'));
    
    // Füge Button hinzu
    Button := TDfmNode.Create('BtnOk', 'TButton');
    Button.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '10'));
    Button.Properties.AddObject('Top', TDfmValue.Create(dvkInteger, '10'));
    Button.Properties.AddObject('Caption', TDfmValue.Create(dvkString, 'OK'));
    Form.AddChild(Button);
    
    // Serialize
    Writer := TDfmWriter.Create;
    try
      Output := Writer.WriteDocument(Doc);
      WriteLn('Generated DFM:');
      WriteLn(Output);
    finally
      Writer.Free;
    end;
    
  finally
    Doc.Free;
  end;
end;

begin
  WriteLn('DFM Parser - Full Test Suite');
  WriteLn('============================');
  WriteLn;
  
  try
    TestBasic;
    WriteLn;
    WriteLn('Press Enter to continue...');
    ReadLn;
    
    TestRoundtrip;
    WriteLn;
    WriteLn('Press Enter to continue...');
    ReadLn;
    
    TestManualCreation;
    WriteLn;
    WriteLn('All tests completed!');
    
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
