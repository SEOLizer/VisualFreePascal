{$mode objfpc}{$H+}
program test_dfm_parser;

{==============================================================================}
{ Test-Programm für DFM Parser                                                }
{==============================================================================}

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

procedure TestBasicParse;
var
  Parser: TDfmParser;
  Document: TDfmDocument;
  Writer: TDfmWriter;
  Input, Output: string;
begin
  WriteLn('=== Test 1: Basic Parse ===');
  
  Input := 
    'object Form1: TForm'#13#10 +
    '  Left = 100'#13#10 +
    '  Top = 200'#13#10 +
    '  Caption = ''Test Form'''#13#10 +
    '  ClientHeight = 300'#13#10 +
    '  object Button1: TButton'#13#10 +
    '    Left = 10'#13#10 +
    '    Top = 10'#13#10 +
    '    Caption = ''Click Me'''#13#10 +
    '  end'#13#10 +
    'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  // Parse
  Parser := TDfmParser.Create(Input);
  try
    Document := Parser.Parse;
    try
      WriteLn('Parse successful!');
      WriteLn('Root: ', Document.Root.InstanceName, ': ', Document.Root.ClassName);
      WriteLn('Children: ', Document.Root.ChildCount);
      
      if Parser.Errors.Count > 0 then
      begin
        WriteLn('Warnings/Errors:');
        for var I := 0 to Parser.Errors.Count - 1 do
          WriteLn('  ', Parser.Errors[I]);
      end;
      
      // Write back
      Writer := TDfmWriter.Create;
      try
        Output := Writer.WriteDocument(Document);
        WriteLn('Output:');
        WriteLn(Output);
      finally
        Writer.Free;
      end;
      
    finally
      Document.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestComplexValues;
var
  Parser: TDfmParser;
  Document: TDfmDocument;
  Writer: TDfmWriter;
  Input, Output: string;
begin
  WriteLn('=== Test 2: Complex Values ===');
  
  Input := 
    'object Form2: TForm'#13#10 +
    '  Anchors = [akLeft, akTop, akRight]'#13#10 +
    '  Visible = True'#13#10 +
    '  Enabled = False'#13#10 +
    '  Tag = 0'#13#10 +
    '  object ListBox1: TListBox'#13#10 +
    '    Items.Strings = <'#13#10 +
    '      item'#13#10 +
    '        Text = ''Item 1'''#13#10 +
    '      end'#13#10 +
    '      item'#13#10 +
    '        Text = ''Item 2'''#13#10 +
    '      end>'#13#10 +
    '  end'#13#10 +
    'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    Document := Parser.Parse;
    try
      WriteLn('Parse successful!');
      
      if Parser.Errors.Count > 0 then
      begin
        WriteLn('Warnings/Errors:');
        for var I := 0 to Parser.Errors.Count - 1 do
          WriteLn('  ', Parser.Errors[I]);
      end;
      
      Writer := TDfmWriter.Create;
      try
        Output := Writer.WriteDocument(Document);
        WriteLn('Output:');
        WriteLn(Output);
      finally
        Writer.Free;
      end;
      
    finally
      Document.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestRoundtrip;
var
  Parser: TDfmParser;
  Document: TDfmDocument;
  Writer: TDfmWriter;
  Input, Output1, Output2: string;
begin
  WriteLn('=== Test 3: Roundtrip ===');
  
  Input := 
    'object Form1: TForm'#13#10 +
    '  Left = 0'#13#10 +
    '  Top = 0'#13#10 +
    '  Width = 640'#13#10 +
    '  Height = 480'#13#10 +
    '  Caption = ''Test Application'''#13#10 +
    '  object Panel1: TPanel'#13#10 +
    '    Left = 10'#13#10 +
    '    Top = 10'#13#10 +
    '    Width = 200'#13#10 +
    '    Height = 100'#13#10 +
    '    object Button1: TButton'#13#10 +
    '      Left = 10'#13#10 +
    '      Top = 10'#13#10 +
    '      Width = 75'#13#10 +
    '      Height = 25'#13#10 +
    '      Caption = ''OK'''#13#10 +
    '      OnClick = Button1Click'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    'end';
  
  WriteLn('Original Input:');
  WriteLn(Input);
  WriteLn;
  
  // Erster Parse
  Parser := TDfmParser.Create(Input);
  try
    Document := Parser.Parse;
    try
      Writer := TDfmWriter.Create;
      try
        Output1 := Writer.WriteDocument(Document);
        WriteLn('First Output:');
        WriteLn(Output1);
        WriteLn;
        
        // Zweiter Parse
        Parser.Free;
        Parser := TDfmParser.Create(Output1);
        Document.Free;
        Document := Parser.Parse;
        Output2 := Writer.WriteDocument(Document);
        
        WriteLn('Second Output:');
        WriteLn(Output2);
        WriteLn;
        
        // Vergleiche
        if Output1 = Output2 then
          WriteLn('✓ Roundtrip successful - outputs are identical!')
        else
          WriteLn('✗ Roundtrip failed - outputs differ');
          
      finally
        Writer.Free;
      end;
    finally
      Document.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestErrors;
var
  Parser: TDfmParser;
  Document: TDfmDocument;
  Input: string;
begin
  WriteLn('=== Test 4: Error Recovery ===');
  
  // Fehlerhafte Eingabe (fehlendes 'end')
  Input := 
    'object Form1: TForm'#13#10 +
    '  Left = 100'#13#10 +
    '  Top =';  // Unvollständig
  
  WriteLn('Input (invalid):');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    Document := Parser.Parse;
    try
      if Assigned(Document) then
      begin
        WriteLn('Document parsed (possibly incomplete)');
        WriteLn('Has Root: ', Assigned(Document.Root));
      end;
      
      WriteLn('Errors found:');
      for var I := 0 to Parser.Errors.Count - 1 do
        WriteLn('  ', Parser.Errors[I]);
        
    finally
      Document.Free;
    end;
  finally
    Parser.Free;
  end;
end;

begin
  WriteLn('DFM Parser Test Suite');
  WriteLn('=====================');
  WriteLn;
  
  try
    TestBasicParse;
    WriteLn;
    WriteLn('Press Enter to continue...');
    ReadLn;
    
    TestComplexValues;
    WriteLn;
    WriteLn('Press Enter to continue...');
    ReadLn;
    
    TestRoundtrip;
    WriteLn;
    WriteLn('Press Enter to continue...');
    ReadLn;
    
    TestErrors;
    WriteLn;
    WriteLn('All tests completed!');
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      WriteLn(E.StackTrace);
    end;
  end;
end.
