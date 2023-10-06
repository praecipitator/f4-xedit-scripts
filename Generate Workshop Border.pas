{
    Run on exterior workshop reference. In theory, this should work on multiple workshops, showing the config UI for each.

    Explanation of the options in the UI:
        - Output File:
                            Output NIF will be written here. Click the ... button to open a "Save File" dialog.
        - Border Height:
                            Determines how far up the border extends above the terrain. Vanilla seems to use 256, but that might look too low, especially on very uneven terrain.
                            Default: 256
        - Border Depth:
                            Determines how far down the border sinks below the terrain. This helps to cover up the imperfections.
                            Default: 128
        - Height Precision:
                            Determines how close the border follows the terrain vertically, lower = closer, higher = looser.
                            The value is the distance between intermediate points along a border segment, a lower value means more points will be created.
                            The distance between the vertices in a cell's heigtmap is 128, so there is probably no need to set it to anything lower.
                            No intermediate points will be created across "flat" terrain.
                            Default: 128
        - Height Tolerance:
                            Determines what "flat" means in regard of the above setting.
                            Height differences below this value will be considered equal.
                            Default: 8

    Special thanks to Jonathan Ostrus for the code to parse exterior cell's terrain
}
unit WorkshopBorder;
    uses praUtil;
    uses CobbLibrary;

    const
        cellCacheFileName = ProgramPath + 'Edit Scripts\Generate Workshop Border - Cell Cache.json';
        configFileName = ProgramPath + 'Edit Scripts\Generate Workshop Border - Config.json';

        primitiveLinkEdid = 'WorkshopLinkedPrimitive';
        SCALE_FACTOR_TERRAIN = 8;

        // svg output stuff
        svgPath = ProgramPath + 'Edit Scripts\';
        targetWidth = 512;
        extraBorder = 8;
        rectSize = 8;

        vertexColorTop = '#00000000';
        vertexColorRed = '#C02D00FF';
        vertexColorGreen = '#00C000FF';
        uvTop = '0.875000 0.171875';
        uvBottom = '0.125000 0.828125';



    var
        heightTolerance: float;
        borderHeight: float;
        borderPrecision: float;
        borderDownHeight: float;
        primitiveLinkKw: IInterface;

        cellHeights: TJsonObject;
        wsOrigin: TJsonObject;
        currentPolygon: TJsonObject; // contains 'edges'.
        debugIndex: integer;
        cellCache: TStringList;
        worldspaceCellCache: TJsonObject;
        saveNifAs: string;
        debugMode: boolean;

    procedure loadConfig();
    var
        configFile: TJsonObject;
    begin
        // defaults
        borderHeight := 256;
        borderPrecision := 128;
        borderDownHeight := 128;
        heightTolerance := 8;
        debugMode := false;

        if(not FileExists(configFileName)) then exit;
        configFile := TJsonObject.create();
        configFile.LoadFromFile(configFileName);

        borderHeight        := configFile.F['borderHeight'];
        borderPrecision     := configFile.F['borderPrecision'];
        borderDownHeight    := configFile.F['borderDownHeight'];
        heightTolerance     := configFile.F['heightTolerance'];
        debugMode           := configFile.B['debugMode'];

        if(borderHeight = 0) then borderHeight := 256;
        if(borderPrecision = 0) then borderPrecision := 128;
        if(borderDownHeight = 0) then borderDownHeight := 128;
        if(heightTolerance = 0) then heightTolerance := 8;

        configFile.free();
    end;

    procedure saveConfig();
    var
        configFile: TJsonObject;
    begin
        configFile := TJsonObject.create();
        configFile.F['borderHeight'] := borderHeight;
        configFile.F['borderPrecision'] := borderPrecision;
        configFile.F['borderDownHeight'] := borderDownHeight;
        configFile.F['heightTolerance'] := heightTolerance;
        configFile.B['debugMode'] := debugMode;

        configFile.SaveToFile(configFileName, false);
        configFile.free();
    end;

    procedure loadCellCache();
    begin
        if(not FileExists(cellCacheFileName)) then exit;
        worldspaceCellCache.LoadFromFile(cellCacheFileName);
    end;

    procedure saveCellCache();
    begin
        worldspaceCellCache.SaveToFile(cellCacheFileName, false);
    end;

    procedure writeSvgHeightmap(cellData: TJsonObject; fName: string);
    var
        outLines: TStringList;
        i: integer;
        minX, minY, maxX, maxY, width, height: float;
        minHeight, maxHeight, scaleFactor, curHeight: float;
        curEdge: TJsonObject;
        x, y: integer;
        xIndex, yIndex, colorTest: string;
    begin
        // rectSize =
        //invert y: SVGs use HTML coordinates
        minX := 0;
        minY := 0;
        maxX := 32 * rectSize;
        maxY := 32 * rectSize;

        minHeight := 900000;
        maxHeight := -900000;
        for x:=0 to 32 do begin
            xIndex := IntToStr(x);
            for y:=0 to 32 do begin
                yIndex := IntToStr(y);
                curHeight := cellData.O[xIndex].F[yIndex];
                if(curHeight < minHeight) then begin
                    minHeight := curHeight;
                end;
                if(curHeight > maxHeight) then begin
                    maxHeight := curHeight;
                end;
            end;
        end;


        width := maxX - minX;
        height := maxY - minY;


        // now scale it somewhat
        // scale = 1024 = targetWidth
        if(width > height) then begin
            scaleFactor := targetWidth / width; // width * x = 1024 ; x = 1024 / width
        end else begin
            scaleFactor := targetWidth / height;
        end;

        scaleFactor := 1;

        width := width * scaleFactor;
        height := height * scaleFactor;

        width := width + extraBorder*2;
        height := height + extraBorder*2;

        outLines := TStringList.create();
        outLines.Add('<svg width="'+IntToStr(width)+'" height="'+IntToStr(height)+'" viewBox="'+IntToStr(minX * scaleFactor)+' '+IntToStr(minY * scaleFactor)+' '+IntToStr(width)+' '+IntToStr(height)+'"');
        outLines.Add('xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"');
        outLines.Add('xmlns:svg="http://www.w3.org/2000/svg"');
        outLines.Add('xmlns="http://www.w3.org/2000/svg">');

        // AddMessage('MinHeight = '+IntToStr(minHeight)+' MaxHeight = '+IntToStr(maxHeight));

        // now output the rects
        for x:=0 to 32 do begin
            xIndex := IntToStr(x);
            for y:=0 to 32 do begin
                yIndex := IntToStr(y);
                curHeight := (cellData.O[xIndex].F[yIndex] - minHeight) / (maxHeight - minHeight) * 255;
                colorTest := IntToHex(curHeight, 2);
                outLines.Add('<rect x="'+IntToStr(x * rectSize)+'" y="'+IntToStr(height - y * rectSize)+'" width="'+IntToStr(rectSize)+'" height="'+IntToStr(rectSize)+'" fill="#'+colorTest+colorTest+colorTest+'" />');
            end;
        end;

        outLines.Add('</svg>');
        outLines.SaveToFile(svgPath+fName);

        outLines.free();
    end;

    procedure writeSvg(poly: TJsonObject; fName: string);
    var
        outLines: TStringList;
        i: integer;
        minX, minY, maxX, maxY, width, height: float;
        curX, curY, scaleFactor: float;
        curEdge: TJsonObject;
    begin
        //invert y: SVGs use HTML coordinates
        minX := 900000;
        minY := 900000;
        maxX := -900000;
        maxY := -900000;

        for i:=0 to poly.A['edges'].count-1 do begin
            curEdge := poly.A['edges'].O[i];

            curX := curEdge.O['a'].F['x'];
            curY := curEdge.O['a'].F['y'] * -1;

            if(curX > maxX) then maxX := curX;
            if(curY > maxY) then maxY := curY;

            if(curX < minX) then minX := curX;
            if(curY < minY) then minY := curY;

            curX := curEdge.O['b'].F['x'];
            curY := curEdge.O['b'].F['y'] * -1;

            if(curX > maxX) then maxX := curX;
            if(curY > maxY) then maxY := curY;

            if(curX < minX) then minX := curX;
            if(curY < minY) then minY := curY;
        end;

        width := maxX - minX;
        height := maxY - minY;

        if(width < 0) then width := width * -1;
        if(height < 0) then height := height * -1;

        // now scale it somewhat
        // scale = 1024 = targetWidth
        if(width > height) then begin
            scaleFactor := targetWidth / width; // width * x = 1024 ; x = 1024 / width
        end else begin
            scaleFactor := targetWidth / height;
        end;



        width := width * scaleFactor;
        height := height * scaleFactor;

        width := width + extraBorder*2;
        height := height + extraBorder*2;

        outLines := TStringList.create();
        outLines.Add('<svg width="'+IntToStr(width)+'" height="'+IntToStr(height)+'" viewBox="'+IntToStr(minX * scaleFactor)+' '+IntToStr(minY * scaleFactor)+' '+IntToStr(width)+' '+IntToStr(height)+'"');
        outLines.Add('xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"');
        outLines.Add('xmlns:svg="http://www.w3.org/2000/svg"');
        outLines.Add('xmlns="http://www.w3.org/2000/svg">');

        // now output the edges
        for i:=0 to poly.A['edges'].count-1 do begin
            curEdge := poly.A['edges'].O[i];
            outLines.Add('<line x1="'+IntToStr(extraBorder + curEdge.O['a'].F['x'] * scaleFactor)+'" y1="'+IntToStr(extraBorder + curEdge.O['a'].F['y'] * scaleFactor * -1)+'" '+
            'x2="'+IntToStr(extraBorder + curEdge.O['b'].F['x'] * scaleFactor)+'" y2="'+IntToStr(extraBorder + curEdge.O['b'].F['y'] * scaleFactor * -1)+'" stroke="#ff0000" stroke-width="8" />');
        end;

        outLines.Add('</svg>');
        outLines.SaveToFile(svgPath+fName);

        outLines.free();
    end;

    function createNifBase(): TJsonObject;
    var
        niHeader, exportInfo, rootNode, transform, bsxFlags, triShape, boundingSphere, VertexDesc: TJsonObject;
        BSEffectShaderProperty, bsEffectCtrler, interpolator, floatData, key: TJsonObject;
        alphaProp, vertex: TJsonObject;
        blockTypes: TJsonArray;
    begin
        Result := TJsonObject.create;
        // header
        niHeader := Result.O['NiHeader'];
        niHeader.S['Magic'] := 'Gamebryo File Format, Version 20.2.0.7';
        niHeader.S['Version'] := '20.2.0.7';
        niHeader.S['Endian Type'] := 'ENDIAN_LITTLE';
        niHeader.S['User Version'] := '12';
        niHeader.S['Num Blocks'] := '8'; // might have to update this
        niHeader.S['User Version 2'] := '130';

        exportInfo := niHeader.O['Export Info'];
        exportInfo.S['Author'] := 'xEdit, pra';
        exportInfo.S['Process Script'] := 'Generate Workshop Border.pas';
        exportInfo.S['Export Script'] := 'Generate Workshop Border.pas';

        niHeader.S['Max Filepath'] := '';// or maybe put something here
        blockTypes := niHeader.A['Block Types'];
        blockTypes.Add('NiNode');
        blockTypes.Add('BSXFlags');
        blockTypes.Add('BSTriShape');
        blockTypes.Add('BSEffectShaderProperty');
        blockTypes.Add('BSEffectShaderPropertyFloatController');
        blockTypes.Add('NiFloatInterpolator');
        blockTypes.Add('NiFloatData');
        blockTypes.Add('NiAlphaProperty');

        blockTypes := niHeader.A['Block Type Index'];
        blockTypes.Add('NiNode');
        blockTypes.Add('BSXFlags');
        blockTypes.Add('BSTriShape');
        blockTypes.Add('BSEffectShaderProperty');
        blockTypes.Add('BSEffectShaderPropertyFloatController');
        blockTypes.Add('NiFloatInterpolator');
        blockTypes.Add('NiFloatData');
        blockTypes.Add('NiAlphaProperty');

        //niHeader.S['Num Strings']  := '4';

        // root node
        rootNode := Result.O['0 NiNode'];
        rootNode.S['Name'] := 'RootNode';
        rootNode.A['Extra Data List'].Add('1 BSXFlags "BSX"');
        rootNode.S['Controller'] := 'None';
        rootNode.S['Flags'] := '14';
        rootNode.S['Collision Object'] := 'None';

        transform := rootNode.O['Transform'];
        transform.S['Translation'] := '0.000000 0.000000 0.000000';
        transform.S['Rotation'] := '0.000000 0.000000 0.000000';
        transform.S['Scale'] := '1.000000';

        rootNode.A['Children'].Add('2 BSTriShape "Border"');

        // BSX flags
        bsxFlags := Result.O['1 BSXFlags'];
        bsxFlags.S['Name'] := 'BSX';
        bsxFlags.S['Flags'] := 'Animated';

        // TriShape
        triShape := Result.O['2 BSTriShape'];
        triShape.S['Name'] := 'Border';
        triShape.S['Controller'] := 'None';
        triShape.S['Flags'] := '14';
        triShape.S['Collision Object'] := 'None';

        transform := triShape.O['Transform'];
        transform.S['Translation'] := '0.000000 0.000000 0.000000';
        transform.S['Rotation'] := '0.000000 0.000000 0.000000';
        transform.S['Scale'] := '1.000000';

        boundingSphere := triShape.O['Bounding Sphere']; // will have to update this
        boundingSphere.S['Center'] := '0.000000 0.000000 0.000000';
        boundingSphere.S['Radius'] := '0.000000';

        triShape.S['Skin'] := 'None';
        triShape.S['Shader Property'] := '3 BSEffectShaderProperty';
        triShape.S['Alpha Property'] := '7 NiAlphaProperty';

        VertexDesc := triShape.O['VertexDesc'];
        VertexDesc.S['VF1'] := '6';
        VertexDesc.S['VF2'] := '2';
        VertexDesc.S['VF3'] := '67';
        VertexDesc.S['VF4'] := '5';
        VertexDesc.S['VF5'] := '0';
        VertexDesc.S['VF'] := 'VF_VERTEX | VF_UV | VF_NORMAL | VF_TANGENT | VF_COLORS';
        VertexDesc.S['VF8'] := '0';

        // looks like I need to have something valid initially
        triShape.S['Num Triangles'] := '1';
        triShape.S['Num Vertices'] := '3';
        triShape.S['Data Size'] := '336';

        vertex := triShape.A['Vertex Data'].addObject();
        vertex.S['Vertex'] := '-0.000006 -96.000000 168.000000';
        vertex.S['Bitangent X'] := '0.000000';
        vertex.S['UV'] := '0.125000 0.171875';
        vertex.S['Normal'] := '1.000000 -0.003922 -0.003922';
        vertex.S['Bitangent Y'] := '1.000000';
        vertex.S['Tangent'] := '-0.003922 -0.003922 -1.000000';
        vertex.S['Bitangent Z'] := '-0.003922';
        vertex.S['Vertex Colors'] := '#00000000';

        vertex := triShape.A['Vertex Data'].addObject();
        vertex.S['Vertex'] := '0.000000 -96.000000 100.000000';
        vertex.S['Bitangent X'] := '0.000000';
        vertex.S['UV'] := '0.125000 0.500000';
        vertex.S['Normal'] := '1.000000 -0.003922 -0.003922';
        vertex.S['Bitangent Y'] := '1.000000';
        vertex.S['Tangent'] := '-0.003922 -0.003922 -1.000000';
        vertex.S['Bitangent Z'] := '-0.003922';
        vertex.S['Vertex Colors'] := '#0040003F';

        vertex := triShape.A['Vertex Data'].addObject();
        vertex.S['Vertex'] := '0.000000 96.000000 100.000000';
        vertex.S['Bitangent X'] := '0.000000';
        vertex.S['UV'] := '0.125000 0.500000';
        vertex.S['Normal'] := '1.000000 -0.003922 -0.003922';
        vertex.S['Bitangent Y'] := '1.000000';
        vertex.S['Tangent'] := '-0.003922 -0.003922 -1.000000';
        vertex.S['Bitangent Z'] := '-0.003922';
        vertex.S['Vertex Colors'] := '#0040003F';

        triShape.A['Triangles'].add('1 2 3');

        BSEffectShaderProperty := Result.O['3 BSEffectShaderProperty'];
        BSEffectShaderProperty.S['Name'] := '';
        BSEffectShaderProperty.S['Controller'] := '4 BSEffectShaderPropertyFloatController';
        BSEffectShaderProperty.S['Shader Flags 1'] := 'GreyscaleToPalette_Alpha | Soft_Effect | ZBuffer_Test';
        BSEffectShaderProperty.S['Shader Flags 2'] := 'Vertex_Colors';
        BSEffectShaderProperty.S['UV Offset'] := '0.000000 0.000000';
        BSEffectShaderProperty.S['UV Scale'] := '20.000000 20.000000';
        BSEffectShaderProperty.S['Source Texture'] := 'textures\Effects\WorkshopBoundary01_d.dds';
        BSEffectShaderProperty.S['Texture Clamp Mode'] := '3';
        BSEffectShaderProperty.S['Lighting Influence'] := '255';
        BSEffectShaderProperty.S['Env Map Min LOD'] := '0';
        BSEffectShaderProperty.S['Unknown Byte'] := '0';
        BSEffectShaderProperty.S['Falloff Start Angle'] := '1.000000';
        BSEffectShaderProperty.S['Falloff Stop Angle'] := '1.000000';
        BSEffectShaderProperty.S['Falloff Start Opacity'] := '0.000000';
        BSEffectShaderProperty.S['Falloff Stop Opacity'] := '0.000000';
        BSEffectShaderProperty.S['Emissive Color'] := '#FFFFFFFF';
        BSEffectShaderProperty.S['Emissive Multiple'] := '1.000000';
        BSEffectShaderProperty.S['Soft Falloff Depth'] := '128.000000';
        BSEffectShaderProperty.S['Grayscale Texture'] := 'Textures\Effects\Gradients\WorkshopBoundaryGrad01.dds';
        BSEffectShaderProperty.S['Env Map Texture'] := '';
        BSEffectShaderProperty.S['Normal Texture'] := '';
        BSEffectShaderProperty.S['Env Mask Texture'] := '';
        BSEffectShaderProperty.S['Environment Map Scale'] := '1.000000';

        bsEffectCtrler := Result.O['4 BSEffectShaderPropertyFloatController'];
        bsEffectCtrler.S['Next Controller'] := 'None';
        bsEffectCtrler.S['Flags'] := 'Active | Compute scaled time';
        bsEffectCtrler.S['Frequency'] := '1.000000';
        bsEffectCtrler.S['Phase'] := '0.000000';
        bsEffectCtrler.S['Start Time'] := '0.000000';
        bsEffectCtrler.S['Stop Time'] := '1.966667';
        bsEffectCtrler.S['Target'] := '3 BSEffectShaderProperty';
        bsEffectCtrler.S['Interpolator'] := '5 NiFloatInterpolator';
        bsEffectCtrler.S['Type of Controlled Variable'] := 'U Offset';

        interpolator := Result.O['5 NiFloatInterpolator'];
        interpolator.S['Pose Value'] := 'Min';
        interpolator.S['Data'] := '6 NiFloatData';

        floatData := Result.O['6 NiFloatData'].O['Data'];
        floatData.S['Num Keys'] := '2';
        floatData.S['Interpolation'] := 'QUADRATIC_KEY';

        key := floatData.A['Keys'].AddObject();
        key.S['Time'] := '0.000000';
        key.S['Value'] := '0.000000';
        key.S['Forward'] := '0.000000';
        key.S['Backward'] := '-1.000000';

        key := floatData.A['Keys'].AddObject();
        key.S['Time'] := '1.966667';
        key.S['Value'] := '-1.000000';
        key.S['Forward'] := '-1.000000';
        key.S['Backward'] := '0.000000';

        alphaProp := Result.O['7 NiAlphaProperty'];
        alphaProp.S['Name'] := '';
        alphaProp.S['Controller'] := 'None';
        alphaProp.S['Flags'] := '4109';
        alphaProp.S['Threshold'] := '64';

        // finally the footer
        Result.O['NiFooter'].A['Roots'].Add('0 NiNode "RootNode"');
    end;

    function findLandInGroup(childGroup: IInterface): IInterface;
    var
        i: integer;
        ref: IInterface;
        sig: string;
    begin
        Result := nil;
        for i := 0 to ElementCount(childGroup)-1 do begin
            ref := ElementByIndex(childGroup, i);
            sig := Signature(ref);
            if(sig = 'LAND') then begin
                Result := ref;
                exit;
            end;
        end;
    end;



    function findLand(cell: IInterface): IInterface;
    var
        tempGroup, permGroup: IInterface;
    begin
        // permGroup := FindChildGroup(ChildGroup(cell), 8, cell); // can LAND even be persistent? I don't think it can
        tempGroup := FindChildGroup(ChildGroup(cell), 9, cell);
        Result := HighestOverrideOrSelf(findLandInGroup(tempGroup), 9000);
    end;

    function getCellHeightmap(cellX, cellY: integer; worldSpace: IInterface): TJsonObject;
    var
        cell, land, vhgt, rows, cols, curRow, curEntry: IInterface;
        x, y: integer;
        xString, yString: string;
        landOffset, landValue, curVal, rowStartVal: float;
    begin
        Result := TJsonObject.create;
        cell := getWorldspaceCell(worldSpace, cellX, cellY, false);
        if(not assigned(cell)) then begin
            AddMessage('Found no cell');
            // todo: something
            exit;
        end;

        land := findLand(cell);
        // works so far
        vhgt := ElementByPath(land, 'VHGT');
        landOffset := GetElementNativeValues(vhgt, 'Offset');
        // contais Offset -> float

        //landValue := landOffset;

        rows := ElementByPath(vhgt, 'Rows');

        for y := 0 to 32 do begin
            yString := IntToStr(y);
            curRow := ElementByIndex(rows, y);
            curRow := ElementByPath(curRow, 'Columns');
            //AddMessage('curRow');
            //dumpElem(curRow);
            for x := 0 to 32 do begin
                xString := IntToStr(x);
                curEntry := ElementByIndex(curRow, x);

                curVal := StrToFloat(GetEditValue(curEntry));
                if(curVal > 127) then begin
                    curVal := curVal - 256;
                end;



                if(x = 0) then begin  // starting new column
                    if(y = 0) then begin // actually the very first value
                        rowStartVal := curVal + landOffset;
                    end else begin
                        rowStartVal := curVal + rowStartVal;
                    end;

                    landValue := rowStartVal;
                end else begin
                    // otherwise, just keep incrementing
                    landValue := landValue + curVal;
                end;


                Result.O[xString].F[yString] := landValue;
            end;
        end;

        // Row = Y
        // Col = X

        // Rows <- iterate that
        //  contains cols, iterate them too
        // AddMessage('Found land: '+FullPath(land));
    end;

    function getTerrainHeight(globalX, globalY: float; worldSpace: IInterface): float;
    var
        cellX, cellY: integer; // cell grid coords
        localX, localY: float; // cell-local coords
        terrainX, terrainY: integer;// coordinates within the terrain grid
        cellGlobalX, cellGlobalY: float;
        cellHeightmap: TJsonObject;
        indexX, indexY, indexTerrainX, indexTerrainY: string;
    begin
        // cellHeights
        cellX := getGridCoord(globalX);
        cellY := getGridCoord(globalY);

        cellGlobalX := getGridCoordInverse(cellX);
        cellGlobalY := getGridCoordInverse(cellY);

        localX := getLocalCellCoord(globalX, cellX);
        localY := getLocalCellCoord(globalY, cellY);


        terrainX := getLandGridCoord(localX);
        terrainY := getLandGridCoord(localY);

        //AddMessage('From Global ('+FloatToStr(globalX)+'/'+FloatToStr(globalY)+'), got cell=('+IntToStr(cellX)+'/'+IntToStr(cellY)+'), local=('+FloatToStr(localX)+'/'+FloatToStr(localY)+'), terrain=('+IntToStr(terrainX)+'/'+IntToStr(terrainY)+'), cell global=('+FloatToStr(cellGlobalX)+'/'+FloatToStr(cellGlobalY)+')');

        indexX := IntToStr(cellX);
        indexY := IntToStr(cellY);
        //decompilerCache.Types[scriptKey] = JSON_TYPE_OBJECT
        if(cellHeights.O[indexX].Types[indexY] <> JSON_TYPE_OBJECT) then begin
            // fill
            AddMessage('Loading terrain from '+IntToStr(cellX)+'/'+IntToStr(cellY));
            cellHeightmap := getCellHeightmap(cellX, cellY, worldSpace);
            if(debugMode) then begin
                writeSvgHeightmap(cellHeightmap, 'cell_'+indexX+'_'+indexY+'.svg');
            end;
            // AddMessage('cellHeightmap='+cellHeightmap.toString());
            cellHeights.O[indexX].O[indexY] := cellHeightmap;
        end;

        indexTerrainX := IntToStr(terrainX);
        indexTerrainY := IntToStr(terrainY);

        Result := (cellHeights.O[indexX].O[indexY].O[indexTerrainX].F[indexTerrainY]) * SCALE_FACTOR_TERRAIN;
        //Result := cellHeights.O[indexX].O[indexY].O[indexTerrainY].F[indexTerrainX];
        // cellHeights.O[IntToStr(cellX)].O[IntToStr(cellY)]
    end;

    function getCachedCell(worldSpace: IInterface; gridX, gridY: integer): IInterface;
    var
        key, wsKey, cellKey: string;
        i: integer;
    begin
        worldSpace := MasterOrSelf(worldSpace);
        wsKey := FormToAbsStr(worldSpace);
        Result := nil;
        if (worldspaceCellCache.Types[wsKey] <> JSON_TYPE_OBJECT) then begin
            // AddMessage('cachedType='+IntToStr(worldspaceCellCache.Types[wsKey]));
            // haven't cached this worldspace yet
            AddMessage('Caching WorldSpace '+FullPath(worldSpace));
            cacheWorldspaceCells(worldSpace);
        end;


        cellKey := worldspaceCellCache.O[wsKey].O[IntToStr(gridX)].S[IntToStr(gridY)];
        if(cellKey = '') then exit;
        Result := AbsStrToForm(cellKey);
        {
        key := IntToStr(gridX)+'_'+IntToStr(gridY);
        i := cellCache.indexOf(key);
        if(i < 0) then exit;

        Result := ObjectToElement(cellCache.Objects[i]);
        }
    end;

    procedure setCachedCell(worldSpace: IInterface; gridX, gridY: integer; cell: IInterface);
    var
        key, wsKey: string;
        i: integer;
        fId: cardinal;
    begin
        fId := FormID(cell);
        if(fId = 0) then exit;

        wsKey := FormToAbsStr(worldSpace);

        // if(worldspaceCellCache.O[wsKey].O[IntToStr(gridX)].Types[IntToStr(gridY)] = JSON_TYPE_STRING)

        worldspaceCellCache.O[wsKey].O[IntToStr(gridX)].S[IntToStr(gridY)] := FormToAbsStr(cell);


        //key := IntToStr(gridX)+'_'+IntToStr(gridY);
        //i := cellCache.indexOf(key);
        //if(i >= 0) then exit;

        //cellCache.AddObject(key, cell);
    end;


    procedure cacheWorldspaceCells(ws: IInterface);
    var
        i, x, y: integer;
        cell: IInterface;
        blockidx, subblockidx, cellidx: integer;
        wrldgrup, block, subblock: IInterface;
        watTest: IInterface;
    begin
        ws := MasterOrSelf(ws);

        wrldgrup := ChildGroup(ws);
        //AddMessage('Do we have worldgrup? '+BoolToStr(assigned(wrldgrup)));
        for blockidx := 0 to ElementCount(wrldgrup)-1 do begin
            block := ElementByIndex(wrldgrup, blockidx);
            //AddMessage('Checking Block #'+IntToStr(blockidx));
            // traverse SubBlocks
            for subblockidx := 0 to ElementCount(block)-1 do begin
                // AddMessage('Checking SubBlock #'+IntToStr(subblockidx));
                subblock := ElementByIndex(block, subblockidx);
                // traverse Cells
                for cellidx := 0 to ElementCount(subblock)-1 do begin
                    cell := ElementByIndex(subblock, cellidx);
                    if(assigned(cell)) then begin
                        x := getElementNativeValues(cell, 'XCLC\X');
                        y := getElementNativeValues(cell, 'XCLC\Y');
                        // AddMessage('Checking cell '+IntToStr(x)+'/'+IntToStr(y));
                        setCachedCell(ws, x, y, cell);
                    end;
                end;
            end;
        end;
        //AddMessage('cached='+worldspaceCellCache.toString());
    end;


    function getWorldspaceCell(ws: IInterface; gridX, gridY: integer; createIfMissing: boolean): IInterface;
    var
        i, x, y: integer;
        cell: IInterface;
        blockidx, subblockidx, cellidx: integer;
        wrldgrup, block, subblock: IInterface;
        watTest: IInterface;
    begin
        cell := getCachedCell(ws, gridX, gridY);
        if(assigned(cell)) then begin
            Result := cell;
            exit;
        end;
        AddMessage('ERROR FAILED');
        {
        //AddMessage('Trying to find in ws='+fullPath(watTest)+' cell '+IntToStr(gridX)+'/'+IntToStr(gridY));
        Result := nil;
        wrldgrup := ChildGroup(watTest); // WTF this doesn't work
        //AddMessage('Do we have worldgrup? '+BoolToStr(assigned(wrldgrup)));
        for blockidx := 0 to ElementCount(wrldgrup)-1 do begin
            block := ElementByIndex(wrldgrup, blockidx);
            //AddMessage('Checking Block #'+IntToStr(blockidx));
            // traverse SubBlocks
            for subblockidx := 0 to ElementCount(block)-1 do begin
                // AddMessage('Checking SubBlock #'+IntToStr(subblockidx));
                subblock := ElementByIndex(block, subblockidx);
                // traverse Cells
                for cellidx := 0 to ElementCount(subblock)-1 do begin
                    cell := ElementByIndex(subblock, cellidx);
                    x := getElementNativeValues(cell, 'XCLC\X');
                    y := getElementNativeValues(cell, 'XCLC\Y');
                    // AddMessage('Checking cell '+IntToStr(x)+'/'+IntToStr(y));
                    setCachedCell(x, y, cell);
                    if(x = gridX) and (y = gridY) then begin
                        Result := cell;
                        exit;
                    end;
                end;
            end;
        end;

        if(createIfMissing) then begin
            Result := Add(ws, 'CELL['+IntToStr(gridX)+','+IntToStr(gridY)+']', true);
        end;
        }
    end;

    {
        Convert a cell-local coord to the cell's VHGT row/col indices
    }
    function getLandGridCoord(localCoord: float): integer;
    begin
        // This is a 33x33 grid, going from 0 to 32
        // the 0 and 32 edges are probably shared with neighbour cells

        // cell's top right    = high Row, high Col     -2/1 | 0/0
        // cell's bottom right = low Row   high Col     -2/2 | 0/1
        // cell's top left     = high Row, low  Col     -1/1 | 1/0
        // cell's bottom left  = low Row,  low  Col     -1/2 | 1/1

        // bottom left -> origin
        // Row = Y
        // Col = X
        Result := localCoord/4096.0 * 32.0;
    end;

    {
        Should return the coordinates of a ref within the cell, from the cell's bottom left
    }
    function getLocalCellCoord(worldCoord: float; cellCoord: integer): float;
    var
        cellBottomLeftCoord: float;
    begin
        // cellCoord shl 12 should give me the coords of the cell's middle
        // subtract half the cell width from that to get bottom left
        // cell width should be 4096, so half is 2048

        cellBottomLeftCoord := getGridCoordInverse(cellCoord);// - 2048;//(cellCoord shl 12) - 2048;
        Result := worldCoord - cellBottomLeftCoord;
    end;

    {
        Returns the corresponding cell grid coordinate for a world coordinate
    }
    function getGridCoord(worldCoord: float): integer;
    var
        numInt: integer;
    begin
        numInt := worldCoord;

        if(numInt < 0) then begin
            Result := getGridCoord(numInt * -1) * -1 - 1;
        end else begin
            Result := numInt shr 12;
        end;
    end;

    function getGridCoordInverse(gridCoord: integer): float;
    var
        numInt: integer;
    begin
        // numInt := worldCoord;

        Result := gridCoord * 4096.0;

        {
        if(gridCoord < 0) then begin
            Result := getGridCoordInverse((1 - (gridCoord+1))) * -1;
        end else begin
            Result := gridCoord shl 12;
        end;
        }
    end;

    function writeVertex(x, y, z: float; isTop, isInner: boolean; vertData: TJsonArray): TJsonObject;
    begin
        Result := vertData.addObject();
        Result.S['Vertex'] := FloatToStr(x)+' '+FloatToStr(y)+' '+FloatToStr(z);
        if(isTop) then begin
            Result.S['UV'] := uvTop;
            Result.S['Vertex Colors'] := vertexColorTop;
        end else begin
            Result.S['UV'] := uvBottom;
            if(isInner) then begin
                Result.S['Vertex Colors'] := vertexColorGreen;
            end else begin
                Result.S['Vertex Colors'] := vertexColorRed;
            end;
        end;
    end;

    procedure writeEdgeToNif(edge, vertData, triData: TJsonArray; reverse: boolean);
    var
        iVert1, iVert2, iVert3, iVert4, iVert5, iVert6: integer;
       // vertData, triData: TJsonObject;
       isInner: boolean;
    begin
        // 2 tris, 4 vertices
        // how do I determine which way they face?
        // let's draw like this:
        {
            3---4
            | \ |
            1---2
            | \ |
            5---6
            A   B
        }
        // with the tris being 1 2 3 and 2 4 3
        //vertData := triShape.A['Vertex Data'];
        //triData := triShape.A['Triangles'];

        iVert1 := vertData.count;
        iVert2 := iVert1 + 1;
        iVert3 := iVert2 + 1;
        iVert4 := iVert3 + 1;
        iVert5 := iVert4 + 1;
        iVert6 := iVert5 + 1;

        isInner := not reverse;

        // 1, a mid
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], edge.O['a'].F['z'] + 0, false, isInner, vertData);
        // 2, b mid
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], edge.O['b'].F['z'] + 0, false, isInner, vertData);
        // 3, a top
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], edge.O['a'].F['z'] + borderHeight, true, isInner, vertData);
        // 4, b top
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], edge.O['b'].F['z'] + borderHeight, true, isInner, vertData);
        // 5, a bottom
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], edge.O['a'].F['z'] - borderDownHeight, false, isInner, vertData);
        // 6, b bottom
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], edge.O['b'].F['z'] - borderDownHeight, false, isInner, vertData);

        if(not reverse) then begin
            // write tris
            // 1 2 3
            triData.Add(IntToStr(iVert1)+' '+IntToStr(iVert2)+' '+IntToStr(iVert3));
            // 2 4 3
            triData.Add(IntToStr(iVert2)+' '+IntToStr(iVert4)+' '+IntToStr(iVert3));
            // 5 6 1
            triData.Add(IntToStr(iVert5)+' '+IntToStr(iVert6)+' '+IntToStr(iVert1));
            // 6 2 1
            triData.Add(IntToStr(iVert6)+' '+IntToStr(iVert2)+' '+IntToStr(iVert1));
        end else begin
            // 1 3 2
            triData.Add(IntToStr(iVert1)+' '+IntToStr(iVert3)+' '+IntToStr(iVert2));
            // 3 4 2
            triData.Add(IntToStr(iVert3)+' '+IntToStr(iVert4)+' '+IntToStr(iVert2));
            // 5 1 6
            triData.Add(IntToStr(iVert5)+' '+IntToStr(iVert1)+' '+IntToStr(iVert6));
            // 6 1 2
            triData.Add(IntToStr(iVert6)+' '+IntToStr(iVert1)+' '+IntToStr(iVert2));
        end;

        // now also write the bottom part

    end;

    procedure writeNif();
    var
        Nif : TwbNifFile;
        nifJson, triShapeJson, vertexDataJson, trianglesJson: TJsonObject;

        triShapeBlock, vertexData, Triangles: TwbNifBlock;
        curEdge: TJsonObject;
        i: integer;
    begin
        // I do not know how to properly create the nif from scratch, so I'm going to create it as JSON, then convert it.
        nifJson := createNifBase();

        //nifJson.
        Nif := TwbNifFile.Create;
        //AddMessage('out nif as string='+nifJson.toString());
        Nif.FromJson(nifJson.toString());

        triShapeBlock := Nif.BlockByName('Border');
        //AddMessage('have triShapeBlock? '+BoolToStr(assigned(triShapeBlock)));


        vertexData := triShapeBlock.Elements['Vertex Data'];
        // vertexData := triShapeBlock.
        // Nif.CreateElement(triShapeBlock) ?? or, what is TdfElement?
        // vertexData := triShapeBlock.AddProperty('Vertex Data');
        //AddMessage('have vertexData? '+BoolToStr(assigned(vertexData)));
        Triangles := triShapeBlock.Elements['Triangles'];
        //AddMessage('have Triangles? '+BoolToStr(assigned(Triangles)));


        // triShapeJson := triShapeBlock.toJson(triShapeBlock);
        vertexDataJson := TJsonObject.parse(vertexData.toJson(vertexData));
        trianglesJson := TJsonObject.parse(Triangles.toJson(Triangles));
        //vertexDataJson := vertexData.toJson(vertexData);
        //trianglesJson := Triangles.toJson(Triangles);

        //AddMessage('vertexDataJson='+vertexDataJson.toString());

        vertexDataJson.clear();
        trianglesJson.clear();

        for i:=0 to currentPolygon.A['edges'].count-1 do begin
            curEdge := currentPolygon.A['edges'].O[i];
            writeEdgeToNif(curEdge, vertexDataJson.A['Vertex Data'], trianglesJson.A['Triangles'], false);
            writeEdgeToNif(curEdge, vertexDataJson.A['Vertex Data'], trianglesJson.A['Triangles'], true);
        end;

        vertexData.fromJson(vertexDataJson.toString());
        Triangles.fromJson(trianglesJson.toString());

        triShapeBlock.EditValues['Num Triangles'] := trianglesJson.A['Triangles'].count;
        triShapeBlock.EditValues['Num Vertices'] := vertexDataJson.A['Vertex Data'].count;

        triShapeBlock.UpdateBounds();

        //triShapeJson.S['Num Triangles'] := triShapeJson.A['Triangles'].count;
        //triShapeJson.S['Num Vertices'] := triShapeJson.A['Vertex Data'].count;

        // https://github.com/TES5Edit/TES5Edit/blob/6a6b9ca787ec1e89f2d634a954cc5b87a7a3630e/Core/wbDataFormatNif.pas#L144
        // UpdateBounds, UpdateNormals, UpdateTangents might work. might be functions of TriShape


        nifJson.free();
        Nif.SpellAddUpdateTangents();
        Nif.SpellUpdateTangents();

        Nif.SaveToFile(saveNifAs);
        Nif.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        debugIndex := 0;
        Result := 0;
        primitiveLinkKw := GetFormByEdid(primitiveLinkEdid);
        cellCache := TStringList.create;
        cellCache.Duplicates := dupIgnore;
        cellCache.Sorted := true;

        cellHeights := TJsonObject.create;
        worldspaceCellCache := TJsonObject.create;
        loadCellCache();

        loadConfig();
    end;

    function getSizeVector(e: IInterface): TJsonObject;
    begin
        Result := newVector(
            StrToFloat(GetElementEditValues(e, 'XPRM\Bounds\X')),
            StrToFloat(GetElementEditValues(e, 'XPRM\Bounds\Y')),
            StrToFloat(GetElementEditValues(e, 'XPRM\Bounds\Z'))
        );
    end;

    function isPointInPolygonEdges(polygonEdges: TJsonArray; testX, testY: float): boolean;
    var
        i, j, numVerts: integer;
        curX, curY: float;
        prevX, prevY: float;
        curEdge: TJsonObject;
    begin
        //numVerts := polygonPoints.count;
        Result := false;
        // ported version of this:
        //https://wrfranklin.org/Research/Short_Notes/pnpoly.html

        // i = 0;
        // j := numVerts-1;
        for i:=0 to polygonEdges.count-1 do begin
            curEdge := polygonEdges.O[i];
            curX := curEdge.O['a'].F['x']; // vertx[i]
            curY := curEdge.O['a'].F['y']; // verty[i]

            prevX := curEdge.O['b'].F['x']; // vertx[j]
            prevY := curEdge.O['b'].F['y']; // verty[j]

            if ((curY>testY) <> (prevY>testY)) then begin
                // separating it, because xedit can't shortcut ifs
                if (testX < (prevX-curX) * (testy-curY) / (prevY-curY) + curX) then begin
                    Result := (not Result);
                end;
            end;

           // j := i;
            //i := i + 1;
        end;
    end;

    function getEdgesFromPoints(points: TJsonArray): TJsonArray;
    var
        i, maxI: integer;
        point1, point2, curEdge: TJsonObject;
    begin
        maxI := points.count-1;
        Result := TJsonArray.create();
        for i:=0 to maxI do begin
            point1 := points.O[i];
            if(i>=maxI) then begin
                point2 := points.O[0];
            end else begin
                point2 := points.O[i+1];
            end;
            curEdge := Result.addObject();
            curEdge.O['a'].F['x'] := point1.O['pos'].F['x'];
            curEdge.O['a'].F['y'] := point1.O['pos'].F['y'];

            curEdge.O['b'].F['x'] := point2.O['pos'].F['x'];
            curEdge.O['b'].F['y'] := point2.O['pos'].F['y'];
        end;
    end;

    function createPolygonData(rawPoints: TJsonArray): TJsonObject;
    var
        i, maxI: integer;
        point1, point2, curEdge, curPoint: TJsonObject;
        points, edges: TJsonArray;
    begin
        maxI := rawPoints.count-1;
        Result := TJsonObject.create();

        //points := Result.A['points'];
        edges := Result.A['edges'];

        for i:=0 to maxI do begin
            point1 := rawPoints.O[i];
            if(i>=maxI) then begin
                point2 := rawPoints.O[0];
            end else begin
                point2 := rawPoints.O[i+1];
            end;
            curEdge := edges.addObject();
            curEdge.O['a'].F['x'] := point1.O['pos'].F['x'];
            curEdge.O['a'].F['y'] := point1.O['pos'].F['y'];

            curEdge.O['b'].F['x'] := point2.O['pos'].F['x'];
            curEdge.O['b'].F['y'] := point2.O['pos'].F['y'];

            //curPoint := points.addObject();
            //curPoint.F['x'] := point1.O['pos'].F['x'];
            //curPoint.F['y'] := point1.O['pos'].F['y'];
        end;
    end;


    function intersectEdgeWithEdge(edge1, edge2: TJsonObject): TJsonObject;
    var
        m1, t1, m2, t2: float;
        xResult, yResult: float;

        x1, x2, y1, y2, s: float; // of edge1
        x3, x4, y3, y4, t: float; // of edge2

        temp1 : float;
    begin
        Result := nil;
        // https://en.wikipedia.org/wiki/Intersection_(geometry)

        x1 := edge1.O['a'].F['x'];
        x2 := edge1.O['b'].F['x'];
        y1 := edge1.O['a'].F['y'];
        y2 := edge1.O['b'].F['y'];

        x3 := edge2.O['a'].F['x'];
        x4 := edge2.O['b'].F['x'];
        y3 := edge2.O['a'].F['y'];
        y4 := edge2.O['b'].F['y'];


        // s*(x2-x1)-t*(x4-x3)=x3-x1
        // s*(y2-y1)-t*(y4-y3)=y3-y1
        // don't dare to try the cramer's rule rn

        // s*(x2-x1)=x3-x1 + t*(x4-x3)
        // s*(y2-y1)=y3-y1 + t*(y4-y3)

        // s=(x3-x1 + t*(x4-x3)) / (x2-x1)
        // s=(y3-y1 + t*(y4-y3)) / (y2-y1)

        // (x3-x1 + t*(x4-x3)) / (x2-x1) = (y3-y1 + t*(y4-y3)) / (y2-y1)
        // (x3-x1 + t*(x4-x3)) * (y2-y1) = (y3-y1 + t*(y4-y3)) * (x2-x1)
        // (x3-x1)*(y2-y1) + t*(x4-x3)*(y2-y1) = (y3-y1)*(x2-x1) + t*(y4-y3)*(x2-x1)
        // t*(x4-x3)*(y2-y1) - t*(y4-y3)*(x2-x1) = (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1)
        // t*((x4-x3)*(y2-y1) - (y4-y3)*(x2-x1)) = (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1)
        // t = ( (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1) ) / ( (x4-x3)*(y2-y1) - (y4-y3)*(x2-x1) )

        temp1 := ( (x4-x3)*(y2-y1) - (y4-y3)*(x2-x1) );
        if(temp1 = 0) then begin
            exit;
        end;

        t := ( (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1) ) / temp1;

        if (x2-x1 = 0) then begin
            if(y2-y1 = 0) then begin
                // AddMessage('Bork occured');
                exit;
            end;
            s := (y3-y1 + t*(y4-y3)) / (y2-y1);
        end else begin
            s := (x3-x1 + t*(x4-x3)) / (x2-x1);
        end;


        // now s and t must be between 0 and 1
        if (0 <= s) and (s <= 1) then begin
            if (0 <= t) and (t <= 1) then begin
                // yes
                xResult := (x1 + s*(x2-x1));
                yResult := (y1 + s*(y2-y1));
                Result := TJsonObject.create;
                Result.F['x'] := xResult;
                Result.F['y'] := yResult;
            end;
        end;
    end;

    function intersectPolyWithEdge(poly, testEdge: TJsonObject): TJsonObject;
    var
        i: integer;
        curEdge, isPoint: TJsonObject;
    begin
        Result := nil;
        for i:=0 to poly.A['edges'].count-1 do begin
            curEdge := poly.A['edges'].O[i];

            isPoint := intersectEdgeWithEdge(curEdge, testEdge);
            if(nil <> isPoint) then begin
                // found
                Result := isPoint;
                exit;
            end;
        end;
    end;

    function intersectPolyWithEdgeMulti(poly, testEdge: TJsonObject): TJsonArray;
    var
        i: integer;
        curEdge, isPoint: TJsonObject;
    begin
        Result := TJsonArray.create();
        for i:=0 to poly.A['edges'].count-1 do begin
            curEdge := poly.A['edges'].O[i];

            isPoint := intersectEdgeWithEdge(curEdge, testEdge);
            if(nil <> isPoint) then begin
                // found one
                appendObjectToArray(Result, isPoint);
                // Result := isPoint;
            end;
        end;
    end;

    procedure appendEdge(edgeArray: TJsonArray; p1, p2: TJsonObject);
    var
        newEdge: TJsonObject;
    begin
        //x1, y1, z1, x2, y2, z2: float
        newEdge := edgeArray.addObject();
        newEdge.O['a'].F['x'] := p1.F['x'];
        newEdge.O['a'].F['y'] := p1.F['y'];
        newEdge.O['a'].F['z'] := p1.F['z'];

        newEdge.O['b'].F['x'] := p2.F['x'];
        newEdge.O['b'].F['y'] := p2.F['y'];
        newEdge.O['b'].F['z'] := p2.F['z'];
    end;

    procedure insertSortPointIntoListRec(sortPoint: TJsonObject; sortedList: TJsonArray; startOffset, endOffset: integer);
    var
        offsetDistance, betweenOffset: integer;
        curPoint1, curPoint2: TJsonObject;
    begin
        if(sortedList.count = 0) then begin
            appendObjectToArray(sortedList, sortPoint);
            exit;
        end;

        offsetDistance := endOffset-startOffset;

        case(offsetDistance) of
            0:  begin
                    // single point, before or after?
                    curPoint1 := sortedList.O[startOffset];
                    if(curPoint1.F['dist'] < sortPoint.F['dist']) then begin
                        // insert after
                        insertObjectIntoArray(sortedList, sortPoint, startOffset+1);
                    end else begin
                        // insert before (insert at, and have everything move forward)
                        insertObjectIntoArray(sortedList, sortPoint, startOffset);
                    end;
                    exit;
                end;
            1:  begin
                    // two points
                    curPoint1 := sortedList.O[startOffset];
                    curPoint2 := sortedList.O[endOffset];
                    if(sortPoint.F['dist'] < curPoint1.F['dist']) then begin
                        // before the first
                        insertObjectIntoArray(sortedList, sortPoint, startOffset);
                    end else if(sortPoint.F['dist'] > curPoint2.F['dist']) then begin
                        // after the second
                        insertObjectIntoArray(sortedList, sortPoint, endOffset+1);
                    end else begin
                        // between the two
                        insertObjectIntoArray(sortedList, sortPoint, endOffset);
                    end;
                    exit;
                end;
        end;

        // otherwise, the proper thing
        betweenOffset := (offsetDistance/2) + startOffset;
        curPoint1 := sortedList.O[betweenOffset];
        if(sortPoint.F['dist'] < curPoint1.F['dist']) then begin
            // between startOffset and betweenOffset
            insertSortPointIntoListRec(sortPoint, sortedList, startOffset, betweenOffset);
        end else begin
            // between betweenOffset and endOffset
            insertSortPointIntoListRec(sortPoint, sortedList, betweenOffset, endOffset);
        end;
    end;

    procedure insertSortPointIntoList(sortPoint: TJsonObject; sortedList: TJsonArray);
    begin
        insertSortPointIntoListRec(sortPoint, sortedList, 0, sortedList.count-1);
    end;

    function sortPointsByDistance(refPoint: TJsonObject; points: TJsonArray): TJsonArray;
    var
        i: integer;
        sortedData: TJsonArray;
        sqDist: float;
        curPoint, newEntry: TJsonObject;
    begin
        sortedData := TJsonArray.create();
        // first, generate the array to sort
        for i:=0 to points.count-1 do begin
            curPoint := points.O[i];
            sqDist := getPointDistanceSq(refPoint, curPoint);
            newEntry := TJsonObject.create();
            newEntry.F['dist'] := sqDist;
            newEntry.F['x'] := curPoint.F['x'];
            newEntry.F['y'] := curPoint.F['y'];

            insertSortPointIntoList(newEntry, sortedData);

            newEntry.free();
        end;

        Result := TJsonArray.create();
        // transform into proper pointlist
        for i:=0 to sortedData.count-1 do begin
            curPoint := sortedData.O[i];
            newEntry := Result.addObject();
            newEntry.F['x'] := curPoint.F['x'];
            newEntry.F['y'] := curPoint.F['y'];
        end;
        sortedData.free();
    end;

    function getPointDistanceSq(p1, p2: TJsonObject): float;
    begin
        Result := sqr(p1.F['x']-p2.F['x']) + sqr(p1.F['y']-p2.F['y']);
    end;

    function getPointDistance(p1, p2: TJsonObject): float;
    begin
        Result := sqrt(getPointDistanceSq(p1, p2));
    end;

    procedure mergePolygonsHalf(poly1, poly2, outPoly: TJsonObject);
    var
        i: integer;
        curEdge, isPoint1, isPoint2, isPoint3, newEdge: TJsonObject;
        pointAin, pointBin: boolean;
        edgesArray, intersectPoints, sortedIntersectPoints: TJsonArray;
    begin
        if(debugMode) then begin
            AddMessage('mergePolygonsHalf BEGIN');
        end;
        // run this twice, with different order of arguments, to generate two halves, then merge
        // outArray := TJsonArray.create();
        edgesArray := outPoly.A['edges'];
        // add poly2 to poly1
        // let's iterate poly2's edges
        for i:=0 to poly2.A['edges'].count - 1 do begin
            if(debugMode) then begin
                AddMessage('Checking edge #'+IntToStr(i));
            end;
            curEdge := poly2.A['edges'].O[i];
            //pointAin := isPointInPolygon(poly1.A['points'], curEdge.O['a'].F['x'], curEdge.O['a'].F['y']);
            //pointBin := isPointInPolygon(poly1.A['points'], curEdge.O['b'].F['x'], curEdge.O['b'].F['y']);

            pointAin := isPointInPolygonEdges(poly1.A['edges'], curEdge.O['a'].F['x'], curEdge.O['a'].F['y']);
            pointBin := isPointInPolygonEdges(poly1.A['edges'], curEdge.O['b'].F['x'], curEdge.O['b'].F['y']);

            intersectPoints := intersectPolyWithEdgeMulti(poly1, curEdge);

            // now: we have curEdge, and n intersectPoints. n should not be > 2
            case (intersectPoints.count) of
                0:  begin
                        if(debugMode) then begin
                            AddMessage('No Intersects');
                        end;
                        // none intersects: either add the edge as-is, or don't
                        //  - both are in:  add nothing
                        //  - both are out: add the edge as-is

                        // current edge does not intersect the poly
                        if(pointAin and pointBin) then begin
                            //intersectPoints.free();
                            // I hope `continue` doesn't interfere with the `case`
                            // continue; // both in, skip
                            if(debugMode) then begin
                                AddMessage(' both in, skipping edge '+curEdge.toString());
                            end;
                        end else if (not pointAin) and (not pointBin) then begin
                            // both out, add
                            if(debugMode) then begin
                                AddMessage(' both out, adding edge '+curEdge.toString());
                            end;
                            appendObjectToArray(edgesArray, curEdge);
                            // continue;
                        end else begin;
                            // this shouldn't happen here
                            AddMessage('!!!ANOMALY: Edge doesn''t intersect, but points are halfway? This makes no sense');
                        end;
                    end;
                1:  begin
                        // AddMessage('One Intersects');
                        // one intersect (IS):
                        //  - A is in:  add IS--B
                        //  - A is out: add A--IS
                        isPoint1 := intersectPoints.O[0];
                        if (pointAin and (not pointBin)) then begin
                            if(debugMode) then begin
                                AddMessage(' IS--B');
                            end;
                            appendEdge(edgesArray, isPoint1, curEdge.O['b']);
                        end else if ((not pointAin) and pointBin) then begin
                            if(debugMode) then begin
                                AddMessage(' A--IS');
                            end;
                            appendEdge(edgesArray, curEdge.O['a'], isPoint1);
                        end else begin
                            AddMessage('!!!ANOMALY: one intersect, but pointAin='+BoolToStr(pointAin)+' pointBin='+BoolToStr(pointBin));
                        end;
                    end;
                else begin
                        if(debugMode) then begin
                            AddMessage('N intersects, n = '+IntToStr(intersectPoints.count));
                        end;

                        //AddMessage('Unsorted list: '+intersectPoints.toString());
                        sortedIntersectPoints := sortPointsByDistance(curEdge.O['a'], intersectPoints);
                        //AddMessage('Sorted list: '+sortedIntersectPoints.toString());
                        if (not pointAin) then begin
                            prependObjectToArray(sortedIntersectPoints, curEdge.O['a']);
                        end;
                        if (not pointBin) then begin
                            appendObjectToArray(sortedIntersectPoints, curEdge.O['b']);
                        end;
                        if ((sortedIntersectPoints.count mod 2) <> 0) then begin
                            AddMessage('ERROR: sortedIntersectPoints ended up at an odd length!');
                            exit;
                        end;

                        //AddMessage('Sorted list with extra points: '+sortedIntersectPoints.toString());
                        i := 0;
                        while(i<sortedIntersectPoints.count) do begin
                            isPoint1 := sortedIntersectPoints.O[i];
                            isPoint2 := sortedIntersectPoints.O[i+1];
                            if(debugMode) then begin
                                AddMessage('Adding edge #'+IntToStr(i)+' '+isPoint1.toString()+' '+isPoint2.toString());
                            end;
                            appendEdge(edgesArray, isPoint1, isPoint2);
                            i := i + 2;
                        end;
                        // damn
                        // first, I'd need a testcase for this
                        // then:
                        //  - sort the intersect points by their distance to A
                        //  - if A is outside, prepend it
                        //  - if B is outside, append it
                        //  - each pair of points (0..1, 2..3, 4..5, ..) is an edge. In theory, this should always be an even nr of points
                        // if this works, replace case 2 with it, too
                        sortedIntersectPoints.free();
                    end;
                //
            end;
            intersectPoints.free();
        end;
    end;

    function mergePolygons(poly1, poly2: TJsonObject): TJsonObject;
    begin
        if(debugMode) then begin
            AddMessage('Merge polygons BEGIN');
            writeSvg(poly1, 'PolyMerge'+IntToStr(debugIndex)+'_poly1.svg');
            writeSvg(poly2, 'PolyMerge'+IntToStr(debugIndex)+'_poly2.svg');
        end;

        Result := TJsonObject.create;

        mergePolygonsHalf(poly2, poly1, Result);

        if(debugMode) then begin
            writeSvg(Result, 'PolyMerge'+IntToStr(debugIndex)+'_firstMerge.svg');
        end;

        mergePolygonsHalf(poly1, poly2, Result);

        if(debugMode) then begin
            writeSvg(Result, 'PolyMerge'+IntToStr(debugIndex)+'_result.svg');
            AddMessage('Merge polygons END');
        end;
        debugIndex := debugIndex + 1;
    end;

    function getBoxPoints(boxPos, boxRot, boxSize: TJsonObject): TJsonArray;
    var
        point1rel, point2rel, point3rel, point4rel: TJsonObject;
        point1abs, point2abs, point3abs, point4abs: TJsonObject;
        zeroRot: TJsonObject;
    begin
        zeroRot := newVector(0,0,0);
        Result := TJsonArray.create();

        {    y
             ^
         4   |   1
             |
        -----+-----> x
             |
         3   |   2
        }

        // point 1:
        point1rel := newVector(
            boxSize.F['x'] / 2,
            boxSize.F['y'] / 2,
            0
        );

        point2rel := newVector(
            boxSize.F['x'] / 2,
            boxSize.F['y'] / 2 * -1,
            0
        );

        point3rel := newVector(
            boxSize.F['x'] / 2 * -1,
            boxSize.F['y'] / 2 * -1,
            0
        );

        point4rel := newVector(
            boxSize.F['x'] / 2 * -1,
            boxSize.F['y'] / 2,
            0
        );

        point1abs := GetCoordinatesRelativeToBase(boxPos, boxRot, point1rel, zeroRot);
        point2abs := GetCoordinatesRelativeToBase(boxPos, boxRot, point2rel, zeroRot);
        point3abs := GetCoordinatesRelativeToBase(boxPos, boxRot, point3rel, zeroRot);
        point4abs := GetCoordinatesRelativeToBase(boxPos, boxRot, point4rel, zeroRot);

        appendObjectToArray(Result, point1abs);
        appendObjectToArray(Result, point2abs);
        appendObjectToArray(Result, point3abs);
        appendObjectToArray(Result, point4abs);


        point1rel.free();
        point2rel.free();
        point3rel.free();
        point4rel.free();
        point1abs.free();
        point2abs.free();
        point3abs.free();
        point4abs.free();
        zeroRot.free();
    end;

    procedure addBox(boxRef: IInterface);
    var
        boxPos, boxRot, boxSize, curPoint: TJsonObject;
        boxPosAdj, curPoly, newPoly: TJsonObject;

        points, edges: TJsonArray;
    begin
        if(GetElementEditValues(boxRef, 'XPRM\Type') <> 'Box') then begin
            AddMessage('Can only process boxes so far: '+FullPath(boxRef));
            exit;
        end;

        boxRot := getRotationVector(boxRef, 'DATA');
        if(boxRot.F['x'] <> 0.0) or (boxRot.F['y'] <> 0.0) then begin
            AddMessage('cannot process boxes rotated on X or Y yet: '+FullPath(boxRef));
            boxRot.free();
            exit;
        end;
        boxSize := getSizeVector(boxRef);
        if(boxSize.F['x'] = 0.0) or (boxSize.F['y'] = 0.0) or (boxSize.F['z'] = 0.0) then begin
            AddMessage('Box seems to have zero size: '+FullPath(boxRef));
            boxSize.free();
            boxRot.free();
            exit;
        end;

        boxPos := getPositionVector(boxRef, 'DATA');
        boxPosAdj := VectorSubtract(boxPos, wsOrigin);

        boxPosAdj.F['z'] := 0;

        points := getBoxPoints(boxPosAdj, boxRot, boxSize);
        //AddMessage('points='+points.toString());

        edges := getEdgesFromPoints(points);

        curPoly := TJsonObject.create;
        curPoly.A['edges'] := edges;
        //AddMessage('curPoly='+curPoly.toString());

        if(currentPolygon.count = 0) then begin
            currentPolygon.free();
            currentPolygon := curPoly;
        end else begin
            newPoly := mergePolygons(curPoly, currentPolygon);
            //AddMessage('merged newPoly='+newPoly.toString());
            curPoly.free();
            currentPolygon.free();
            currentPolygon := newPoly;
        end;


        // now we need:
        // [x] the points of the box, rotated (in the future, maybe even a 2d projection of the arbitrary rotated box)
        // [x] an algorithm to actually calculate the union of a set of polygons
        // [?] assemble the edges I found into a polygon with an inside and an outside
        // [x] read the LAND record, find the height for each edge.
        // [x]add more points along the path
        // [x] actually write a NIF based on this...



        points.free();
        boxSize.free();
        boxRot.free();
        boxPos.free();
        boxPosAdj.free();
    end;

    procedure addIntermediatePoints(worldSpace: IInterface; point1, point2: TJsonObject; edgesArray: TJsonArray; xOffset, yOffset, zOffset: float);
    var
        distance, distanceTarget, curHeight, prevHeight: float;
        i, numIntermediates: integer;
        prevPoint, curPoint, tempPoint: TJsonObject;

        curDistsance, t, curX, curY, curZ: float;
    begin
        //borderPrecision
        //heightTolerance
        distance := getPointDistance(point1, point2);

        numIntermediates := distance/borderPrecision - 1;
        //        prevPoint := //point1;
        prevPoint := cloneJsonObject(point1);
        //curHeight := getTerrainHeight(xOffset + point1.F['x'], yOffset + point1.F['y'], worldSpace) - zOffset;
        //point1.F['z'] := curHeight;
        curHeight := point1.F['z'];

        prevHeight := curHeight;

        curDistsance := 0;
        // https://math.stackexchange.com/questions/175896/finding-a-point-along-a-line-a-certain-distance-away-from-another-point
        for i:=0 to numIntermediates-1 do begin
            curDistsance := borderPrecision * (i+1);
            // d_t is borderPrecision
            t := curDistsance/distance;

            curX := (1-t)*point1.F['x'] + t*point2.F['x'];
            curY := (1-t)*point1.F['y'] + t*point2.F['y'];
            curZ := getTerrainHeight(xOffset + curX, yOffset + curY, worldSpace) - zOffset;
            if(floatEqualsWithTolerance(prevHeight, curZ, heightTolerance)) then continue; // skip this

            prevHeight := curZ;
            curPoint := TJsonObject.create;
            curPoint.F['x'] := curX;
            curPoint.F['y'] := curY;
            curPoint.F['z'] := curZ;

            // otherwise add prevPoint and curPoint
            appendEdge(edgesArray, prevPoint, curPoint);
            prevPoint.free();
            prevPoint := curPoint;
        end;

        // finally add prevPoint and point2
        appendEdge(edgesArray, prevPoint, point2);
        prevPoint.free();

    end;

    procedure addTerrainHeight(workShop, worldSpace: IInterface);
    var
        i: integer;
        curEdge, point1, point2: TJsonObject;
        curHeight, wsX, wsY, wsZ: float;
        newPolygon, prevPolygon: TJsonObject;
    begin
        newPolygon := TJsonObject.create;
        //borderPrecision

        wsX := GetElementNativeValues(workShop, 'DATA\Position\X');
        wsY := GetElementNativeValues(workShop, 'DATA\Position\Y');
        wsZ := GetElementNativeValues(workShop, 'DATA\Position\Z');

        // will this edit in-place, or return? We'll see
        for i:=0 to currentPolygon.A['edges'].count-1 do begin
            curEdge := currentPolygon.A['edges'].O[i];

            point1 := cloneJsonObject(curEdge.O['a']);
            point2 := cloneJsonObject(curEdge.O['b']);

            point1.F['z'] := getTerrainHeight(wsX + point1.F['x'], wsY + point1.F['y'], worldSpace) - wsZ;
            point2.F['z'] := getTerrainHeight(wsX + point2.F['x'], wsY + point2.F['y'], worldSpace) - wsZ;

            addIntermediatePoints(worldSpace, point1, point2, newPolygon.A['edges'], wsX, wsY, wsZ);

            point1.free();
            point2.free();
        end;

        prevPolygon := currentPolygon;
        currentPolygon := newPolygon;
        prevPolygon.free();
    end;



    function getWorkshopDescription(wsRef: IInterface): string;
    var
        location: IInterface;
    begin
        location := pathLinksTo(wsRef, 'XLRL');
        if(assigned(location)) then begin
            Result := ShortName(wsRef) + ' at ' + getElementEditValues(location, 'FULL');
            exit;
        end;
        Result := ShortName(wsRef);
    end;

    procedure outputBrowseHandler(Sender: TObject);
	var
		inputPath: TEdit;
		pathStr: string;
	begin
		inputPath := TLabeledEdit(sender.parent.FindComponent('editOutput'));
		pathStr := ShowSaveFileDialog('Save Output As', 'NIF Files|*.nif|All Files|*.*');
		if(pathStr <> '') then begin
            if(not strEndsWithCI(pathStr, '.nif')) then begin
                pathStr := pathStr + '.nif';
            end;
			inputPath.Text := pathStr;
		end;
	end;

    procedure cacheClearHandler(Sender: TObject);
	var
		btnClearCache: TButton;
        cacheLabel: TLabel;
	begin
		btnClearCache := TButton(sender.parent.FindComponent('btnClearCache'));
		cacheLabel := TLabel(sender.parent.FindComponent('cacheLabel'));

        worldspaceCellCache.clear();

        AddMessage('Cache cleared.');

        btnClearCache.enabled := false;
        cacheLabel.Text := 'Cell Cache Empty';
	end;


    function showGui(wsRef: IInterface): boolean;
    var
        frm: TForm;
        btnOk, btnCancel, btnBrowse, btnClearCache: TButton;
		editOutput, editHeightTop, editHeightBottom, editPrecision, editTolerance: TLabeledEdit;
        resultCode: cardinal;
        cacheLabel: TLabel;
    begin
        Result := false;
        frm := CreateDialog('Generate Workshop Border', 450, 300);
        CreateLabel(frm, 10, 10, 'Processing: '+getWorkshopDescription(wsRef));

        editOutput := CreateLabelledInput(frm, 10, 50, 380, 50, 'Output File:', '');
        editOutput.Name := 'editOutput';
        editOutput.Text := '';
        btnBrowse := CreateButton(frm, 400, 48, '...');
        btnBrowse.onclick := outputBrowseHandler;

        {borderHeight: float;
        borderPrecision: float;
        borderDownHeight: float;}

        editHeightTop := CreateLabelledInput(frm, 10, 100, 160, 50, 'Border Height', FloatToStr(borderHeight));
        editHeightBottom := CreateLabelledInput(frm, 200, 100, 160, 50, 'Border Depth', FloatToStr(borderDownHeight));


        editPrecision := CreateLabelledInput(frm, 10, 140, 160, 50, 'Height Precision', FloatToStr(borderPrecision));
        editTolerance := CreateLabelledInput(frm, 200, 140, 160, 50, 'Height Tolerance', FloatToStr(heightTolerance));

        cacheLabel := CreateLabel(frm, 10, 174, 'Cell Cache Empty');
        cacheLabel.Name := 'cacheLabel';
        btnClearCache := CreateButton(frm, 200, 170, 'Clear Cell Cache');
        btnClearCache.Name := 'btnClearCache';
        btnClearCache.enabled := false;
        btnClearCache.onclick := cacheClearHandler;

        if(worldspaceCellCache.count > 0) then begin
            btnClearCache.enabled := true;
            cacheLabel.Text := 'Cell Cache Loaded';
        end;

        btnOk     := CreateButton(frm, 130, 210, '  OK  ');
        btnCancel := CreateButton(frm, 210, 210, 'Cancel');

        btnOk.Default := true;
        btnOk.ModalResult := mrYes;

        btnCancel.ModalResult := mrCancel;


        {
        saveNifAs = 'F:\MO2-Games\Fallout4\mods\Pond Settlement DEV\Meshes\pra\PondBorder.nif'; // DEBUG

        borderHeight = 512;
        borderPrecision = 128;
        borderDownHeight = 128;
        }
        resultCode := frm.ShowModal();
        if(resultCode = mrYes) then begin
            borderHeight := StrToFloat(editHeightTop.Text);
            borderPrecision := StrToFloat(editPrecision.Text);
            borderDownHeight := StrToFloat(editHeightBottom.Text);

            saveNifAs := trim(editOutput.Text);
            if(saveNifAs <> '') then begin
                Result := true;
            end;
            saveConfig();
        end;

        frm.free();


    end;
    
    function isCellInterior(cell: IInterface): boolean;
    var
        dataFlags: cardinal;
    begin
        // interior flag: 1
        dataFlags := GetElementNativeValues(cell, 'DATA');
        Result := (dataFlags and 1) <> 0;
    end;

    procedure processWorkshop(wsRef: IInterface);
    var
        boxes: TList;
        i: integer;
        watTest, worldSpace, wsCell: IInterface;
    begin
        wsCell := pathLinksTo(wsRef, 'CELL');
        if(isCellInterior(wsCell)) then exit;
        // find the worldspace of this wsRef
        worldSpace := pathLinksTo(wsCell, 'Worldspace');

        if(not assigned(worldSpace)) or (Signature(worldSpace) <> 'WRLD') then begin
            if(debugMode) then begin
                AddMessage('Failed to get worldspace from '+FullPath(wsRef));
            end;
            exit;
        end;

        boxes := getLinkedRefChildren(wsRef, primitiveLinkKw);
        if(boxes.count = 0) then begin,
            if(debugMode) then begin
                AddMessage('Found no building area boxes, nothing to do.');
            end;
            boxes.free();
            exit;
        end;
        AddMessage('Processing Workshop: ' + FullPath(wsRef));
        if (not showGui(wsRef)) then begin
            exit;
        end;
        wsOrigin := getPositionVector(wsRef, 'DATA');

        currentPolygon := TJsonObject.create;

        //polygonEdges := TJsonObject.create;
        AddMessage('Building 2D polygon out of '+IntToStr(boxes.count)+' build area primitives.');
        for i:=0 to boxes.count-1 do begin
            addBox(ObjectToElement(boxes[i]));
            // if(i >= 1) then break; // DEBUG
        end;

        // AddMessage('Output='+currentPolygon.toString());
        if(debugMode) then begin
            writeSvg(currentPolygon, 'output.svg');
        end;

        //AddMessage('Caching Cells');
        //cacheWorldspaceCells(worldSpace);
        AddMessage('Adding Terrain Height');
        addTerrainHeight(wsRef, worldSpace);

        AddMessage('Writing NIF');
        writeNif();
        AddMessage('Finished!');

        wsOrigin.free();
        //polygonEdges.free();

        currentPolygon.free();
        boxes.free();
    end;

    procedure testReference(e: IInterface);
    var
        refX, refY, terrainHeight: float;
        refWs: IInterface;
    begin
        refX := GetElementNativeValues(e, 'DATA\Position\X');
        refY := GetElementNativeValues(e, 'DATA\Position\Y');

        refWs := pathLinksTo(pathLinksTo(e, 'CELL'), 'Worldspace');

        terrainHeight := getTerrainHeight(refX, refY, refWs);

        AddMessage('For ref at '+FloatToStr(refX)+'/'+FloatToStr(refY)+' got height: '+FloatToStr(terrainHeight));
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        processWorkshop(e);

        // test something else
        //testReference(e);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
        if(cellCache <> nil) then begin
            cellCache.free();
        end;

        saveCellCache();

        cellHeights.free();
        worldspaceCellCache.free();
    end;

end.