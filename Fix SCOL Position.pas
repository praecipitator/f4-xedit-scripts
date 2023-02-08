{
    Run on an SCOL, it will ask you for 3 coordinates, and will move everything in it by that offset
}
unit FixScolPosition;    

    uses praUtil;

    var
        inputOffsetX, inputOffsetY, inputOffsetZ : float;

    
        
    function showPositionInput(curEdid: string): boolean;
    var
        offsetList: TStringList;
    begin
        Result := true;
        
        offsetList := ShowVectorInput('Fix SCOL Position', 'Input the position offset to adjust current SCOL'+#10#13+'EDID: '+curEdid, inputOffsetX, inputOffsetY, inputOffsetZ);
        
        if(offsetList = nil) then begin
            Result := false;
            exit;
        end;
        
        inputOffsetX := StrToFloat(offsetList[0]);
        inputOffsetY := StrToFloat(offsetList[1]);
        inputOffsetZ := StrToFloat(offsetList[2]);
        
        offsetList.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        
        inputOffsetX := 0.0;
        inputOffsetY := 0.0;
        inputOffsetZ := 0.0;
    end;
    
    procedure modProperty(e: IInterface; path: string; modifier: float);
    var
        origVal: float;
    begin
        origVal := StrToFloat(GetEditValue(ElementByPath(e, path)));
        
        SetEditValue(ElementByPath(e, path), FloatToStr(origVal+modifier));
    end;   

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        curEdid: string;
        i, lastIndex: integer;
        partsRoot, curPart: IInterface;
        
        curX, curY, curZ: float;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        AddMessage('Processing: ' + FullPath(e));

        // processing code goes here
        if(signature(e) <> 'SCOL') then begin
            exit;
        end;
        
        curEdid := EditorID(e);
        if(not showPositionInput(curEdid)) then begin
            AddMessage(curEdid+' cancelled by user.');
            exit;
        end;
        
        if (inputOffsetX = 0) and (inputOffsetY = 0) and (inputOffsetZ = 0) then begin
            AddMessage('(0/0/0) given as asset, skipping');
            exit;
        end;
        
        AddMessage('Applying offset ('+FloatToStr(inputOffsetX)+'/'+FloatToStr(inputOffsetY)+'/'+FloatToStr(inputOffsetZ)+') to '+curEdid);
        
        partsRoot := ElementByPath(e, 'Parts');
        lastIndex := ElementCount(partsRoot)-1;
        for i := 0 to lastIndex do begin
            curPart := ElementByIndex(partsRoot, i);
            //dumpElem(curPart);
            
            curX := StrToFloat(GetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\X')));
            curY := StrToFloat(GetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\Y')));
            curZ := StrToFloat(GetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\Z')));
            
            if (curX = 0) and (curY = 0) and (curZ = 0) then begin
                //AddMessage('Skipping last element');
            end else begin
                // adjust
                SetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\X'), FloatToStr(curX+inputOffsetX));
                SetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\Y'), FloatToStr(curY+inputOffsetY));
                SetEditValue(ElementByPath(curPart, 'DATA\Placement\Position\Z'), FloatToStr(curZ+inputOffsetZ));
            end;
        end;
        
        // apply it to OBND, too
        modProperty(e, 'OBND\X1', floor(inputOffsetX));
        modProperty(e, 'OBND\Y1', floor(inputOffsetY));
        modProperty(e, 'OBND\Z1', floor(inputOffsetZ));
        
        modProperty(e, 'OBND\X2', ceil(inputOffsetX));
        modProperty(e, 'OBND\Y2', ceil(inputOffsetY));
        modProperty(e, 'OBND\Z2', ceil(inputOffsetZ));
        
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;        
    end;

end.