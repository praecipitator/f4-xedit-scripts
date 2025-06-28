{
    Workshop Border Generation Script v2.1

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! =========================================== IMPORTANT ============================================ !!!
    !!! For this to work, you MUST disable the xEdit setting "Simple records LAND, NAVI, NAVM, CELL, WRLD" !!!
    !!! It's found in the ≡ menu in the top left of xEdit. Or press Ctrl+O.                                !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Run on exterior workshop reference. In theory, this should work on multiple workshops, showing the config UI for each.
    NOTICE: please make sure your workshop areas have some overlap. The script doesn't work property if corners or edges are eactly aligned with each other.

    The resulting NIF is intended to be positioned at the workshop's position, but without any rotation.
    TIP: select the Border, press Ctrl+Shift+A, and click the Workshop to align it.

    Required Scripts:
        - praUtil.pas
        - CobbLibrary.pas
        - XeditSimpleMath.pas

    Short explanation:
        The script combines all the workshop's build areas into a single shape, then walks along that shape's edge, taking fixed-length steps.
        At each step, it tries to find the terrain's height, and places a new point. Out of these points, the border mesh is generated.
        
    Changelog:
        2.1:    - Attempted to fix the issue where the script would malfunction if one corner of a box was located right on the edge of another.
        2.0:    - Fixed several issues with complex settlements made out of multiple build areas which don't all overlap with each other.
                - The script now supports disjunct build areas, that is, when build areas don't overlap with each other at all.

    Explanation of the options in the UI:
        - Output File:
                            Output NIF will be written here. Click the ... button to open a "Save File" dialog.
        - Border Height:
                            Determines how far up the border extends above the terrain. Vanilla seems to use 256, but that might look too low, especially on very uneven terrain.
                            Default: 256
        - Border Depth:
                            Determines how far down the border sinks below the terrain. This helps to cover up the imperfections.
                            Default: 128
        - Step Size:
                            Determines how many intermediate points the script creates along the border.
                            Smaller step size means more intermediate points are created, with less distance in-between, meaning the border will follow the terrain more closely.
                            Higher step size means less intermediate points are created, with more distance in-between, meaning the border will follow the terrain less closely.
                            The distance between the vertices in a cell's heigtmap is 128, so there is probably no need to set this to anything lower.
                            No intermediate points will be created across "flat" terrain.
                            Default: 128
        - Height Tolerance:
                            Determines what "flat" means in regard of the above setting.
                            Height differences below this value will be considered equal, and no intermediate points are created if the current step's height is equal to the previous value.
                            Default: 16

        - Extend to Bottom of Build Area:
                            The border will extend downwards to the lowest point of the Workshop's build area at least.
                            Default: true

        - Use Cell Water Height:
                            If the cell's water height (not any water planes!) is above the terrain, it will be used instead of the terrain height.
                            Default: true

        - Soft Bottom Edge:
                            The bottom edge of the border will fade out, like the top edge. It will be solid color otherwise.
                            Might be useful together with "Use Cell Water Height": false and "Extend to Bottom of Build Area": false
                            Default: false

        - Flatten Bottom Edge:
                            The border will extend downwards to it's lowest point.
                            Default: false

        - Full Precision:
                            The border mesh will be generated with Full Precision vertices.
                            The coordinates of Full Precision meshes are more exact, but they cannot be part of SCOLs.
                            Default: false

        - Auto Output File:
                            If enabled, the script will try to pre-fill the "Output File" field, based on the Workshop's EditorID and/or location.
                            If checked while the UI is open and the "Output File" field is empty, it will be auto-filled as well.
                            The path will be something like "Fallout 4\Data\Meshes\WorkshopBorders\MyWorkshopRef.nif"
                            Default: true


        - Height Override Marker (Editor ID):
                            Editor ID of an Activator base form which should be considered as the "Height Override Markers". If left open, this feature isn't used.
                            See below for explanation.
                            Default: n/a

    Using Height Override Markers:
        You can place "Height Override Markers" along the build area edge, to override the terrain's height in certain places,
        where certain static objects extend too high above the terrain, for example.

        Howto:
            1. Create a new Activator without any model (easiest way: make a copy of DefaultEmptyTrigger).
                Note the EditorID of this new Activator.
            2. Place as many instances of this Activator as needed along the edge of the build area. They should appear as box primitives.
                Position these boxes in such a way, that the bottom faces are where you want the height to be.
            3. When running the script, put the EditorID from Step 1 into the field "Height Override Marker (Editor ID)".

        Notes:
            - You may position or rotate the boxes on the Z axis without any restrictions.
            - For resizing, make sure the box doesn't have zero size in any dimension, such boxes will be skipped.
            - For rotating along the X or Y axes, do not rotate it by 90° or more. This might cause unexpected behavior.
            - If two or more boxes overlap, the script will just pick one or the other, without any guarantee which gets picked when.
            - The border will not necessarily be aligned with the boxes. The script will use the height from the helper box if a step falls within the box,
                but it doesn't care about where the box ends.

    "Hidden" Settings:
        These settings are not exposed in the UI, but can be changed in the config JSON:

            - debugMode (boolean):
                        If true, the script will output more messages, and also generate some SVG files in the Edit Scripts directory
                        Default: false

            - autoPathBase (string):
                        Used to generate the path if "Auto Output File" is enabled.
                        Will be appended to the game's Data path.
                        You MUST use two \\ instead of one here!
                        Default: "Meshes\\WorkshopBorders\\"

    Special thanks to Jonathan Ostrus for the code to parse exterior cell's terrain.
}
unit WorkshopBorder;
    uses praUtil;
    uses CobbLibrary;

    const
        cellCacheFileName = ScriptsPath + 'Generate Workshop Border - Cell Cache.json';
        configFileName = ScriptsPath + 'Generate Workshop Border - Config.json';

        primitiveLinkEdid = 'WorkshopLinkedPrimitive';
        SCALE_FACTOR_TERRAIN = 8;

        // svg output stuff
        svgPath = ScriptsPath;
        targetWidth = 512;
        extraBorder = 8;
        rectSize = 8;

        vertexColorTop = '#00000000';
        vertexColorRed = '#C02D00FF';
        vertexColorGreen = '#00C000FF';
        uvTop = '0.875000 0.171875';
        uvBottom = '0.125000 0.828125';
        FLAG_FULL_PREC = 16384;
        DEFAULT_AUTO_PATH_BASE = 'Meshes\WorkshopBorders\';

        EPSILON = 0.0001;



    var
        heightTolerance: float;
        borderHeight: float;
        borderPrecision: float;
        borderDownHeight: float;
        primitiveLinkKw: IInterface;
        currentGuiWorkshopRef: IInterface;

        helperBoxes: TJsonObject;
        cellHeights: TJsonObject;
        wsOrigin: TJsonObject;
        wsMinHeight: float;
        borderMinHeight: float;
        currentPolygon: TJsonObject; // contains 'edges'.
        debugIndex: integer;
        cellCache: TStringList;
        worldspaceCellCache: TJsonObject;
        saveNifAs: string;
        debugMode: boolean;
        extendToBottom: boolean;
        useWaterHeight: boolean;
        softBottomEdge: boolean;
        flattenBottomEdge: boolean;
        autoOutputFile: boolean;
        doFullPrec: boolean;
        heightHelperBoxEdid: string;
        autoPathBase: string;
        haveAnomalies: boolean;
        terrainBreakingXEditVersion: cardinal;

    procedure loadConfig();
    var
        configFile: TJsonObject;
    begin
        // defaults
        borderHeight := 256;
        borderPrecision := 128;
        borderDownHeight := 128;
        heightTolerance := 16;
        debugMode := false;
        extendToBottom := true;
        useWaterHeight := true;
        softBottomEdge := false;
        flattenBottomEdge := false;
        autoOutputFile := true;
        doFullPrec := false;
        heightHelperBoxEdid := '';
        autoPathBase := DEFAULT_AUTO_PATH_BASE;

        if(not FileExists(configFileName)) then exit;
        configFile := TJsonObject.create();
        configFile.LoadFromFile(configFileName);

        borderHeight        := getJsonObjectValueVariant(configFile, 'borderHeight', borderHeight);
        borderPrecision     := getJsonObjectValueVariant(configFile, 'borderPrecision', borderPrecision);
        borderDownHeight    := getJsonObjectValueVariant(configFile, 'borderDownHeight', borderDownHeight);
        heightTolerance     := getJsonObjectValueVariant(configFile, 'heightTolerance', heightTolerance);
        debugMode           := getJsonObjectValueVariant(configFile, 'debugMode', debugMode);

        extendToBottom      := getJsonObjectValueVariant(configFile, 'extendToBottom', extendToBottom);
        useWaterHeight      := getJsonObjectValueVariant(configFile, 'useWaterHeight', useWaterHeight);
        softBottomEdge      := getJsonObjectValueVariant(configFile, 'softBottomEdge', softBottomEdge);
        flattenBottomEdge   := getJsonObjectValueVariant(configFile, 'flattenBottomEdge', flattenBottomEdge);
        autoOutputFile      := getJsonObjectValueVariant(configFile, 'autoOutputFile', autoOutputFile);
        doFullPrec          := getJsonObjectValueVariant(configFile, 'doFullPrec', doFullPrec);
        heightHelperBoxEdid := trim(getJsonObjectValueVariant(configFile, 'heightHelperBoxEdid', heightHelperBoxEdid));
        autoPathBase        := trim(getJsonObjectValueVariant(configFile, 'autoPathBase', autoPathBase));

        if(borderHeight <= 0) then borderHeight := 256;
        if(borderPrecision <= 0) then borderPrecision := 128;
        if(borderDownHeight <= 0) then borderDownHeight := 128;
        if(heightTolerance <= 0) then heightTolerance := 16;
        if(autoPathBase = '') then autoPathBase := DEFAULT_AUTO_PATH_BASE;

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
        configFile.B['extendToBottom'] := extendToBottom;
        configFile.B['useWaterHeight'] := useWaterHeight;
        configFile.B['softBottomEdge'] := softBottomEdge;
        configFile.B['flattenBottomEdge'] := flattenBottomEdge;
        configFile.B['autoOutputFile'] := autoOutputFile;
        configFile.B['doFullPrec'] := doFullPrec;
        configFile.S['heightHelperBoxEdid'] := heightHelperBoxEdid;
        configFile.S['autoPathBase'] := autoPathBase;

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
                if ((maxHeight - minHeight) = 0) then begin
                    curHeight := minHeight;
                end else begin
                    curHeight := (cellData.O[xIndex].F[yIndex] - minHeight) / (maxHeight - minHeight) * 255.0;
                end;
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

        point1X, point1Y, point2X, point2Y: integer;
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

        if(width = 0) then width := 10;
        if(height = 0) then height := 10;

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

            point1X := extraBorder + curEdge.O['a'].F['x'] * scaleFactor;
            point1Y := extraBorder + curEdge.O['a'].F['y'] * scaleFactor * -1;
            point2X := extraBorder + curEdge.O['b'].F['x'] * scaleFactor;
            point2Y := extraBorder + curEdge.O['b'].F['y'] * scaleFactor * -1;

            outLines.Add('<line x1="'+IntToStr(point1X)+'" y1="'+IntToStr(point1Y)+'" '+
            'x2="'+IntToStr(point2X)+'" y2="'+IntToStr(point2Y)+'" stroke="#ff0000" stroke-width="8" />');

            outLines.Add('<text x="'+IntToStr(point1X)+'" y="'+IntToStr(point1Y)+'" class="small">('+FloatToStr(curEdge.O['a'].F['x'])+'/'+FloatToStr(curEdge.O['a'].F['y'])+')</text>');
            outLines.Add('<text x="'+IntToStr(point2X)+'" y="'+IntToStr(point2Y)+'" class="small">('+FloatToStr(curEdge.O['b'].F['x'])+'/'+FloatToStr(curEdge.O['b'].F['y'])+')</text>');
        end;

        outLines.Add('</svg>');
        outLines.SaveToFile(svgPath+fName);

        outLines.free();
    end;

    procedure writeSvgClusters(clusters: TJsonArray; fName: string);
    var
        outLines: TStringList;
        i, j, k: integer;
        minX, minY, maxX, maxY, width, height: float;
        curX, curY, scaleFactor: float;
        curEdge, curPoint, prevPoint: TJsonObject;
        curCluster, curBox: TJsonArray;
        clusterColors: TSTringList;
        curColor: string;

        point1X, point1Y, point2X, point2Y: integer;
    begin
        //invert y: SVGs use HTML coordinates
        minX := 900000;
        minY := 900000;
        maxX := -900000;
        maxY := -900000;


        for i:=0 to clusters.count-1 do begin
            curCluster := clusters.A[i];
            for j:=0 to curCluster.count-1 do begin
                curBox := curCluster.A[j];
                for k:=0 to curBox.count-1 do begin
                    curPoint := curBox.O[k];

                    curX := curPoint.O['pos'].F['x'];
                    curY := curPoint.O['pos'].F['y'] * -1;

                    if(curX > maxX) then maxX := curX;
                    if(curY > maxY) then maxY := curY;

                    if(curX < minX) then minX := curX;
                    if(curY < minY) then minY := curY;
                end;
            end;
        end;


        width := maxX - minX;
        height := maxY - minY;

        if(width < 0) then width := width * -1;
        if(height < 0) then height := height * -1;

        if(width = 0) then width := 10;
        if(height = 0) then height := 10;

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

        clusterColors := TStringList.create();
        clusterColors.Add('#ff0000');
        clusterColors.Add('#00ff00');
        clusterColors.Add('#0000ff');
        clusterColors.Add('#ffff00');
        clusterColors.Add('#ff00ff');
        clusterColors.Add('#00ffff');

        // now output the points as edges.. somehow
        for i:=0 to clusters.count-1 do begin
            curCluster := clusters.A[i];
            curColor := clusterColors[i mod clusters.count];
            for j:=0 to curCluster.count-1 do begin
                curBox := curCluster.A[j];


                for k:=1 to curBox.count do begin
                    // write this box
                    prevPoint := curBox.O[k-1];
                    if(k = curBox.count) then begin
                        curPoint := curBox.O[0];
                    end else begin
                        curPoint := curBox.O[k];
                    end;

                    point1X := extraBorder + curPoint.O['pos'].F['x'] * scaleFactor;
                    point1Y := extraBorder + curPoint.O['pos'].F['y'] * scaleFactor * -1;

                    point2X := extraBorder + prevPoint.O['pos'].F['x'] * scaleFactor;
                    point2Y := extraBorder + prevPoint.O['pos'].F['y'] * scaleFactor * -1;


                    outLines.Add('<line x1="'+IntToStr(point1X)+'" y1="'+IntToStr(point1Y)+'" '+
                        'x2="'+IntToStr(point2X)+'" y2="'+IntToStr(point2Y)+'" stroke="'+curColor+'" stroke-width="8" />');

                    outLines.Add('<text x="'+IntToStr(point1X)+'" y="'+IntToStr(point1Y)+'" class="small">('+FloatToStr(curPoint.O['pos'].F['x'])+'/'+FloatToStr(curPoint.O['pos'].F['y'])+')</text>');
                    outLines.Add('<text x="'+IntToStr(point2X)+'" y="'+IntToStr(point2Y)+'" class="small">('+FloatToStr(prevPoint.O['pos'].F['x'])+'/'+FloatToStr(prevPoint.O['pos'].F['y'])+')</text>');
                end;
            end;
        end;

        outLines.Add('</svg>');
        outLines.SaveToFile(svgPath+fName);

        clusterColors.free();
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
        vertex.S['Vertex'] := '-0.123456 -96.000000 168.000000';
        vertex.S['Bitangent X'] := '0.000000';
        vertex.S['UV'] := '0.125000 0.171875';
        vertex.S['Normal'] := '1.000000 -0.003922 -0.003922';
        vertex.S['Bitangent Y'] := '1.000000';
        vertex.S['Tangent'] := '-0.003922 -0.003922 -1.000000';
        vertex.S['Bitangent Z'] := '-0.003922';
        vertex.S['Vertex Colors'] := '#00000000';

        vertex := triShape.A['Vertex Data'].addObject();
        vertex.S['Vertex'] := '0.000000 -96.123456 100.000000';
        vertex.S['Bitangent X'] := '0.000000';
        vertex.S['UV'] := '0.125000 0.500000';
        vertex.S['Normal'] := '1.000000 -0.003922 -0.003922';
        vertex.S['Bitangent Y'] := '1.000000';
        vertex.S['Tangent'] := '-0.003922 -0.003922 -1.000000';
        vertex.S['Bitangent Z'] := '-0.003922';
        vertex.S['Vertex Colors'] := '#0040003F';

        vertex := triShape.A['Vertex Data'].addObject();
        vertex.S['Vertex'] := '0.000000 96.000000 100.123456';
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
        xString, yString, entryString: string;
        landOffset, landValue, curVal, rowStartVal: float;
        waterHeight, landHeightScaled: float;
    begin
        Result := TJsonObject.create;
        cell := getWorldspaceCell(worldSpace, cellX, cellY);
        if(not assigned(cell)) then begin
            AddMessage('ERROR! Found no cell!');
            AddMessage('You can try deleting ' + cellCacheFileName);
            exit;
        end;

        if(useWaterHeight) then begin
            if(getElementEditValues(cell, 'XCLW') <> 'Default') then begin
                waterHeight := getElementNativeValues(cell, 'XCLW');
            end else begin
                waterHeight := getElementNativeValues(worldSpace, 'DNAM\Default Water Height');
            end;
        end;

        land := findLand(cell);
        // works so far
        vhgt := ElementByPath(land, 'VHGT');
        landOffset := GetElementNativeValues(vhgt, 'Offset');
        // contais Offset -> float

        //landValue := landOffset;

        if (wbVersionNumber >= terrainBreakingXEditVersion) then begin
            rows := ElementByPath(vhgt, 'Height Data');
        end else begin
            rows := ElementByPath(vhgt, 'Rows');
        end;


        for y := 0 to 32 do begin
            yString := IntToStr(y);
            curRow := ElementByIndex(rows, y);

            if (wbVersionNumber < terrainBreakingXEditVersion) then begin
                curRow := ElementByPath(curRow, 'Columns');
            end;
            for x := 0 to 32 do begin
                xString := IntToStr(x);
                curEntry := ElementByIndex(curRow, x);

                entryString := GetEditValue(curEntry);
                if(entryString = '') then begin
                    AddMessage('Either you didn''t disable the "Simple Records" setting in xEdit, or xEdit broke reading Landscape records again. This script will crash now.');
                end;
                curVal := StrToFloat(entryString);
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

                landHeightScaled := landValue * SCALE_FACTOR_TERRAIN;

                if(useWaterHeight) then begin
                    if(landHeightScaled > waterHeight) then begin
                        Result.O[xString].F[yString] := landHeightScaled;
                    end else begin
                        Result.O[xString].F[yString] := waterHeight;
                    end;
                end else begin
                    Result.O[xString].F[yString] := landHeightScaled;
                end
            end;
        end;

        // Row = Y
        // Col = X

        // Rows <- iterate that
        //  contains cols, iterate them too
        // AddMessage('Found land: '+FullPath(land));
    end;

    function getHeightFrom3points(point1, point2, point3: TJsonObject; atX, atY: float): TJsonObject;
    var
        s, t, det: float;
    begin
        Result := nil;
        // plane equation: X = A + s * (B-A) + t * (C-A)

        // atX = x1 + s * (x2 - x1) + t * (x3 - x1)
        // atY = y1 + s * (y2 - y1) + t * (y3 - y1)
        // atZ = z1 + s * (z2 - z1) + t * (z3 - z1)

        // (atX - x1 - s * (x2 - x1))/(x3 - x1) = t
        // (atY - y1 - s * (y2 - y1))/(y3 - y1) = t
        // (atX - x1 - s * (x2 - x1))/(x3 - x1) = (atY - y1 -  s * (y2 - y1))/(y3 - y1)
        // (atX - x1 - s * (x2 - x1))*(y3 - y1) = (atY - y1 -  s * (y2 - y1))*(x3 - x1)

        // atX*(y3 - y1) - x1*(y3 - y1) - s*(x2 - x1)*(y3 - y1) = atY*(x3 - x1) - y1*(x3 - x1) -  s*(y2 - y1)*(x3 - x1)

        //    s*(y2 - y1)*(x3 - x1) - s*(x2 - x1)*(y3 - y1) = atY*(x3 - x1) - y1*(x3 - x1)  - atX*(y3 - y1) + x1*(y3 - y1)

        //    s*((y2 - y1)*(x3 - x1) - (x2 - x1)*(y3 - y1)) = atY*(x3 - x1) - y1*(x3 - x1) - atX*(y3 - y1) + x1*(y3 - y1)

        // damn I hope I didn't make any mistake...

        //    s = (atY*(x3 - x1) - y1*(x3 - x1) - atX*(y3 - y1) + x1*(y3 - y1))/((y2 - y1)*(x3 - x1) - (x2 - x1)*(y3 - y1))
        det := ((point2.F['y'] - point1.F['y'])*(point3.F['x'] - point1.F['x']) - (point2.F['x'] - point1.F['x'])*(point3.F['y'] - point1.F['y']));
        if(det = 0) then begin
            AddMessage('ERROR: division by zero while processing a height override box! This shouldn''t happen...');
            //Result := getHeightFrom3pointsAlt(point1, point2, point3, atX, atY);
            exit;
        end;
        s := (atY*(point3.F['x'] - point1.F['x']) - point1.F['y']*(point3.F['x'] - point1.F['x']) - atX*(point3.F['y'] - point1.F['y']) + point1.F['x']*(point3.F['y'] - point1.F['y']))/det;
        // I think division by zero shouldn't happen, as I made sure the boxes don't have zero-size
        t := (atX - point1.F['x'] - s * (point2.F['x'] - point1.F['x']))/(point3.F['x'] - point1.F['x']);

        // finally
        Result := TJsonObject.create;
        Result.F['z'] := point1.F['z'] + s * (point2.F['z'] - point1.F['z']) + t * (point3.F['z'] - point1.F['z']);
        //AddMessage('Found this: '+FloatToStr(Result.F['z']));
    end;

    function getHelperBoxHeight(globalX, globalY: float; worldSpace: IInterface): TJsonObject;
    var
        wsKey: string;
        edges, wsEntry, box: TJsonObject;
        i: integer;
        pointsArray, edgesArray: TJsonArray;
    begin


        // wrapping in a json object so I can tell 0 from none
        Result := nil;

        wsKey := FormToAbsStr(worldSpace);
        wsEntry := helperBoxes.O['worldspaces'].A[wsKey];
        // helperBoxes
        for i:=0 to wsEntry.count-1 do begin
            box := wsEntry.O[i];
            pointsArray := box.A['points'];
            {    y
                 ^
             4   |   1
                 |
            -----+-----> x
                 |
             3   |   2
            }
            {
            // we don't even need all 4
            point1 := pointsArray.O[0];
            point2 := pointsArray.O[1];
            point3 := pointsArray.O[2];
            point4 := pointsArray.O[3];
            }
            // TODO: add bounding box test as a pre-check

            if(isPointInPolygonEdges(box.A['edges'], globalX, globalY)) then begin
                Result := getHeightFrom3points(pointsArray.O[0], pointsArray.O[1], pointsArray.O[2], globalX, globalY);

            end;

            if(assigned(Result)) then exit;
        end;
    end;

    function getTerrainHeight(globalX, globalY: float; worldSpace: IInterface): float;
    var
        cellX, cellY: integer; // cell grid coords
        localX, localY: float; // cell-local coords
        terrainX, terrainY: integer;// coordinates within the terrain grid
        cellGlobalX, cellGlobalY: float;
        cellHeightmap: TJsonObject;
        indexX, indexY, indexTerrainX, indexTerrainY: string;
        boxHeight: TJsonObject;
    begin
        // should the helperbox override the terrain, or only count if it's > than terrain?
        boxHeight := getHelperBoxHeight(globalX, globalY, worldSpace);

        if(boxHeight <> nil) then begin
            Result := boxHeight.F['z'];
            boxHeight.free();
            exit;
        end;


        // cellHeights
        cellX := getGridCoord(globalX);
        cellY := getGridCoord(globalY);

        cellGlobalX := getGridCoordInverse(cellX);
        cellGlobalY := getGridCoordInverse(cellY);

        localX := getLocalCellCoord(globalX, cellX);
        localY := getLocalCellCoord(globalY, cellY);


        terrainX := getLandGridCoord(localX);
        terrainY := getLandGridCoord(localY);


        indexX := IntToStr(cellX);
        indexY := IntToStr(cellY);

        if(cellHeights.O[indexX].Types[indexY] <> JSON_TYPE_OBJECT) then begin
            // fill
            AddMessage('Loading terrain from '+IntToStr(cellX)+'/'+IntToStr(cellY));
            cellHeightmap := getCellHeightmap(cellX, cellY, worldSpace);
            if(cellHeightmap.count = 0) then begin
                AddMessage('Failed to load terrain for '+FloatToStr(globalX)+'/'+FloatToStr(globalY)+', returning 0');
                Result := 0;
            end else begin
                if(debugMode) then begin
                    writeSvgHeightmap(cellHeightmap, 'cell_'+indexX+'_'+indexY+'.svg');
                end;
            end;
            // AddMessage('cellHeightmap='+cellHeightmap.toString());
            cellHeights.O[indexX].O[indexY] := cellHeightmap;
        end;

        indexTerrainX := IntToStr(terrainX);
        indexTerrainY := IntToStr(terrainY);

        Result := (cellHeights.O[indexX].O[indexY].O[indexTerrainX].F[indexTerrainY]);
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
            // haven't cached this worldspace yet
            AddMessage('Caching WorldSpace '+FullPath(worldSpace));
            cacheWorldspaceCells(worldSpace);
        end;


        cellKey := worldspaceCellCache.O[wsKey].O[IntToStr(gridX)].S[IntToStr(gridY)];
        if(cellKey = '') then exit;
        Result := AbsStrToForm(cellKey);
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

        worldspaceCellCache.O[wsKey].O[IntToStr(gridX)].S[IntToStr(gridY)] := FormToAbsStr(cell);
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


    function getWorldspaceCell(ws: IInterface; gridX, gridY: integer): IInterface;
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
        AddMessage('ERROR: FAILED to get cell '+IntToStr(gridX)+'/'+IntToStr(gridY)+' in worldspace '+FullPath(ws));
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
        Result := gridCoord * 4096.0;
    end;

    function writeVertex(x, y, z: float; isTop, isInner: boolean; vertData: TwbNifBlock): TwbNifBlock;
    var
        newVertex: TwbNifBlock;
    begin
        Result := vertData.Add();
        Result.EditValues['Vertex'] := formatFloatForNif(x)+' '+formatFloatForNif(y)+' '+formatFloatForNif(z);
        // not sure if my bit-fuckery is any faster than xedit's own sting to half-prec float, and I might have made a mistake
        // surprisingly, this function seems to generate the same vertex coordinates as xEdit's own half-prec float conversion
        // writeHalfPrecVertexCoordsManually(x, y, z, Result);


        if(isTop) then begin
            Result.EditValues['UV'] := uvTop;
            Result.EditValues['Vertex Colors'] := vertexColorTop;
        end else begin
            Result.EditValues['UV'] := uvBottom;
            if(isInner) then begin
                Result.EditValues['Vertex Colors'] := vertexColorGreen;
            end else begin
                Result.EditValues['Vertex Colors'] := vertexColorRed;
            end;
        end;

    end;

    function writeTriangle(i1, i2, i3: integer; triData: TwbNifBlock): TwbNifBlock;
    begin
        Result := triData.Add();
        Result.EditValue := IntToStr(i1)+' '+IntToStr(i2)+' '+IntToStr(i3);
    end;

    procedure writeEdgeToNif(edge: TJsonArray; vertData, triData: TwbNifBlock; reverse: boolean);
    var
        iVert1, iVert2, iVert3, iVert4, iVert5, iVert6: integer;
       // vertData, triData: TJsonObject;
       isInner: boolean;

       baseZa, bottomZa, topZa: float;
       baseZb, bottomZb, topZb: float;
    begin
        // 2 tris, 4 vertices
        // let's draw like this:
        {
            3---4 <- Top
            | \ |
            1---2 <- Baseline
            | \ |
            5---6 <- Bottom
            ^   ^
            A   B
        }

        iVert1 := vertData.count;
        iVert2 := iVert1 + 1;
        iVert3 := iVert2 + 1;
        iVert4 := iVert3 + 1;
        iVert5 := iVert4 + 1;
        iVert6 := iVert5 + 1;

        isInner := not reverse;

        baseZa := edge.O['a'].F['z'];
        bottomZa := edge.O['a'].F['z'] - borderDownHeight;
        topZa := edge.O['a'].F['z'] + borderHeight;

        baseZb := edge.O['b'].F['z'];
        bottomZb := edge.O['b'].F['z'] - borderDownHeight;
        topZb := edge.O['b'].F['z'] + borderHeight;

        if (flattenBottomEdge) then begin
            bottomZa := borderMinHeight;
            bottomZb := borderMinHeight;
        end;

        if (extendToBottom) then begin
            if(bottomZa > wsMinHeight) then bottomZa := wsMinHeight;
            if(bottomZb > wsMinHeight) then bottomZb := wsMinHeight;
        end;

        // 1, a mid
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], baseZa, false, isInner, vertData);
        // 2, b mid
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], baseZb, false, isInner, vertData);
        // 3, a top
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], topZa, true, isInner, vertData);
        // 4, b top
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], topZb, true, isInner, vertData);
        // 5, a bottom
        writeVertex(edge.O['a'].F['x'], edge.O['a'].F['y'], bottomZa, softBottomEdge, isInner, vertData);
        // 6, b bottom
        writeVertex(edge.O['b'].F['x'], edge.O['b'].F['y'], bottomZb, softBottomEdge, isInner, vertData);


        if(not reverse) then begin
            // write tris
            // 1 2 3
            writeTriangle(iVert1, iVert2, iVert3, triData);
            // 2 4 3
            writeTriangle(iVert2, iVert4, iVert3, triData);
            // 5 6 1
            writeTriangle(iVert5, iVert6, iVert1, triData);
            // 6 2 1
            writeTriangle(iVert6, iVert2, iVert1, triData);
        end else begin
            // 1 3 2
            writeTriangle(iVert1, iVert3, iVert2, triData);
            // 3 4 2
            writeTriangle(iVert3, iVert4, iVert2, triData);
            // 5 1 6
            writeTriangle(iVert5, iVert1, iVert6, triData);
            // 6 1 2
            writeTriangle(iVert6, iVert1, iVert2, triData);
        end;
    end;

    function formatFloatForNif(f: float): string;
    begin
        Result := FormatFloat('0.000000', f);
    end;

    function assembleHalfPrec(sgn, exp, val: cardinal): cardinal;
    begin
        Result := ((sgn shl $F) and $8000) or ((exp shl $A) and $7C00) or (val and $3FF);
    end;

    // 16 bits of the result number matter, the left-most 8 ones are the msb
    function floatToHalfPrec(f: float): cardinal;
    var
        sgn, exp, val: cardinal;
        sign, e, testVal: integer;
        tempFloat: float;
    begin
        // AddMessage('recieved f = '+FloatToStr(f));
        if(f = 0) then begin
            // special case: just return all bits set to 0
            Result := 0;
            // AddMessage('first exit at 0 '+IntToStr(Result));
            exit;
        end;
        sgn := 0;
        if(f < 0) then begin
            sgn := 1;
            f := f * -1;
        end;
        // how would I know if I need to set the expontent to 0? probably by plain number size. these seem to be the "subnormal numbers"
        // the largest subnormal number is 2^(-14) * (1023/1024). how much can pascal even store?
        // 0,000060975551605224609375
        if(f <= 0.000060975551605224609375) then begin
            // so we're subnormal. this means the equation is
            // f = 2^(-14) * (x/1024) where x is > 0 and < 1024.
            // f / (2^(-14)) = x/1024
            // x = 1024 * f / (2^(-14))

            Result := assembleHalfPrec(sgn, 0, 1024 * f / Power(2, -14));
            exit;
        end;

        // how do I know the expontent anyway?
        // now the equation is:
        // f = 2^e * (1 + x/1024)
        // where we need to find both e and x
        // we know that 0 < x < 1024, and -14 <= e <= 15, but does it help?
        // we could literally iterate all the exponents until we find an x in range...
        for e:=-14 to 15 do begin
            // equation is
            // f = 2^e * (1 + x/1024)
            // f/2^e = 1 + x/1024
            // f/2^e - 1 = x/1024
            // (f/2^e - 1)*1024 = x

            tempFloat := (f/Power(2.0, e) - 1.0) * 1024.0;
            // important to check this as float, the number might be too huge for an integer
            if (tempFloat > 0) and (tempFloat < 1024) then begin
                testVal := tempFloat;
                exp := e+15;
                val := testVal;

                Result := assembleHalfPrec(sgn, exp, val);
                exit;
            end;
        end;

        Result := 0;
    end;

    function halfPrecToFloat(msB, lsB: cardinal): float;
    var
        combined, sgn, exp, val: cardinal;

        exponent, sign, x: integer;
    begin
        //AddMessage('Input: '+IntToStr(msb)+' '+IntToStr(lsB));
        combined := (msB shl 8) or lsB;
        //AddMessage('Combined: '+IntToStr(combined));
        sgn := (combined and $8000) shr $F; // 1000 0000 0000 0000
        exp := (combined and $7C00) shr $A; // 0111 1100 0000 0000
        val := (combined and $3FF);         // 0000 0011 1111 1111

        // AddMessage('SGN = '+IntToStr(sgn)+' EXP='+IntToStr(exp)+' VAL='+IntToStr(val));
        // SGN = 1 EXP=11 VAL=999

        exponent := exp-15;
        if(exponent < -14) then exponent := -14;
        // -4

        sign := 1;
        if(sgn = 1) then sign := -1;

        x := 1;
        if(exp = 0) then x := 0;

        Result := sign * Power(2, exponent) * (x + val/1024);
    end;

    {
        Deprecated
    procedure writeHalfPrecVertexCoordsManually(x, y, z: float; vertex: TwbNifBlock);
    var
        encX, encY, encZ: cardinal;
        encXM, encXL, encYM, encYL, encZM, encZL: float;
        something: variant;
    begin
        // AddMessage(IntToStr(floatToHalfPrec(x)));
        encX := floatToHalfPrec(x);
        encY := floatToHalfPrec(y);
        encZ := floatToHalfPrec(z);

        encXM := (encX shr 8) and $FF;
        encXL := encX and $FF;

        encYM := (encY shr 8) and $FF;
        encYL := encY and $FF;

        encZM := (encZ shr 8) and $FF;
        encZL := encZ and $FF;

        // write order: x, y, z, lsB first
        // damn I hope it will let me write this
        something := vertex.NativeValues['Vertex'];
        something[0] := encXL;
        something[1] := encXM;
        something[2] := encYL;
        something[3] := encYM;
        something[4] := encZL;
        something[5] := encZM;

        vertex.NativeValues['Vertex'] := something;
    end;
    }

    procedure ClearTriangleData(Triangles: TwbNifBlock);
    var
        entry: TwbNifBlock;
    begin
        while Triangles.count > 0 do begin
            entry := Triangles[0];
            // AddMessage('wat '+entry.EditValue);
            entry.remove();
        end;
    end;

    procedure ClearVertexData(vertData: TwbNifBlock);
    var
        vert: TwbNifBlock;
    begin

        while vertData.count > 0 do begin
            vert := vertData[0];
            vert.remove();
        end;
    end;

    procedure writeNifFromPolygons(polygons: TJsonArray);
    var
        Nif : TwbNifFile;
        nifJson, triShapeJson, vertexDataJson, trianglesJson: TJsonObject;

        triShapeBlock, vertexData, Triangles, vertexDesc: TwbNifBlock;
        curEdge: TJsonObject;
        i, j: integer;
        nifDir: string;

        curPoly: TJsonObject;
    begin

        nifJson := createNifBase();

        Nif := TwbNifFile.Create;
        //AddMessage('out nif as string='+nifJson.toString());
        Nif.FromJson(nifJson.toString());

        triShapeBlock := Nif.BlockByName('Border');

        vertexData := triShapeBlock.Elements['Vertex Data'];
        Triangles := triShapeBlock.Elements['Triangles'];

        // direct nif object code

        ClearVertexData(vertexData);
        ClearTriangleData(Triangles);
        if (doFullPrec) then begin
            vertexDesc := triShapeBlock.Elements['VertexDesc'];
            vertexDesc.NativeValues['VF'] := (vertexDesc.NativeValues['VF'] or FLAG_FULL_PREC);
        end;

        for j:=0 to polygons.count-1 do begin
            curPoly := polygons.O[j];
            for i:=0 to curPoly.A['edges'].count-1 do begin
                curEdge := curPoly.A['edges'].O[i];
                writeEdgeToNif(curEdge, vertexData, Triangles, false);
                writeEdgeToNif(curEdge, vertexData, Triangles, true);
            end;
        end;

        triShapeBlock.EditValues['Num Triangles'] := Triangles.count;
        triShapeBlock.EditValues['Num Vertices'] := vertexData.count;

        triShapeBlock.UpdateBounds();

        // https://github.com/TES5Edit/TES5Edit/blob/6a6b9ca787ec1e89f2d634a954cc5b87a7a3630e/Core/wbDataFormatNif.pas#L144
        // UpdateBounds, UpdateNormals, UpdateTangents might work. might be functions of TriShape


        nifJson.free();
        Nif.SpellAddUpdateTangents();
        Nif.SpellUpdateTangents();

        // ensure path
        nifDir := ExtractFilePath(saveNifAs);
        if (not DirectoryExists(nifDir)) then begin
            if (not ForceDirectories(nifDir)) then begin
                AddMessage('ERROR: failed to create directory '+nifDir);
                Nif.free();
                exit;
            end;
        end;

        Nif.SaveToFile(saveNifAs);
        Nif.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        if(PRA_UTIL_VERSION < 16.0) then begin
            AddMessage('This requires praUtil.pas version 16.0 or higher.');
            Result := 1;
            exit;
        end;

        terrainBreakingXEditVersion := xEditVersionToCardinal(4, 1, 5, 'h');

        debugIndex := 0;
        Result := 0;
        primitiveLinkKw := GetFormByEdid(primitiveLinkEdid);
        cellCache := TStringList.create;
        cellCache.Duplicates := dupIgnore;
        cellCache.Sorted := true;

        cellHeights := TJsonObject.create;
        helperBoxes := TJsonObject.create;
        worldspaceCellCache := TJsonObject.create;
        loadCellCache();

        loadConfig();

        haveAnomalies := false;
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
        {
            int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
            |*
              int i, j, c = 0;
              for (i = 0, j = nvert-1; i < nvert; j = i++) |*
                if ( ((verty[i]>testy) != (verty[j]>testy)) &&
                 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
                   c = !c;
              *|
              return c;
            *|

        }

        // i = 0;
        // j := numVerts-1;
        for i:=0 to polygonEdges.count-1 do begin
            curEdge := polygonEdges.O[i];

            {
            // try this shortcut
            if(isPointOnEdgeFloats(curEdge, testX, testY)) then begin
                Result := false;
                exit;
            end;
            }

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

    {
        TODO: should I actually use this, instead of the other function?
    
    function isPointOnOrInPolygonEdges(polygonEdges: TJsonArray; testX, testY: float): boolean;
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

            // try this shortcut
            if(isPointOnEdgeFloats(curEdge, testX, testY)) then begin
                Result := true;
                exit;
            end;

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
    

    function isPointOnPolygonEdges(polygonEdges: TJsonArray; testX, testY: float): boolean;
    var
        i, j, numVerts: integer;
        curX, curY: float;
        prevX, prevY: float;
        curEdge: TJsonObject;
    begin

        Result := false;

        for i:=0 to polygonEdges.count-1 do begin
            curEdge := polygonEdges.O[i];

            if(isPointOnEdgeFloats(curEdge, testX, testY)) then begin
                Result := true;
                exit;
            end;

        end;
    end;
    }

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

    function getMinOf4(x1, x2, x3, x4: float): float;
    begin
        if(x1 < x2) then begin
            Result := x1;
        end else begin
            Result := x2;
        end;

        if(x3 < Result) then begin
            Result := x3;
        end;

        if(x4 < Result) then begin
            Result := x4;
        end;
    end;

    function getMaxOf4(x1, x2, x3, x4: float): float;
    begin
        if(x1 > x2) then begin
            Result := x1;
        end else begin
            Result := x2;
        end;

        if(x3 > Result) then begin
            Result := x3;
        end;

        if(x4 > Result) then begin
            Result := x4;
        end;
    end;

    function isValueBetween(value1, value2, valueToCheck: float): boolean;
    begin
        if(value1 < value2) then begin
            Result := (value1 <= valueToCheck) and (valueToCheck <= value2);
        end else begin
            Result := (value2 <= valueToCheck) and (valueToCheck <= value1);
        end;
    end;

    function isPointOnEdgeFloats(edge: TJsonObject; x, y: float): boolean;
    var
        m, t, temp: float;
    begin
        Result := false;
        // y := mx + t
        // y1 = m*x1 + t
        // y2 = m*x2 + t
        // y1 - m*x1 = y2 - m*x2
        // m*x1 - m*x2 = y1 - y2
        // m*(x1-x2) = y1-y2
        // m = (y1-y2)/(x1-x2)

        if floatEquals(edge.O['a'].F['x'], edge.O['b'].F['x']) then begin
            // vertical line

            if floatEquals(edge.O['a'].F['x'], x) then begin
                Result := isValueBetween(edge.O['a'].F['y'], edge.O['b'].F['y'], y);
            end;
            exit;
        end;

        if floatEquals(edge.O['a'].F['y'], edge.O['b'].F['y']) then begin

            // horizontal line
            if floatEquals(edge.O['a'].F['y'], y) then begin
                Result := isValueBetween(edge.O['a'].F['x'], edge.O['b'].F['x'], x);
            end;
            exit;
        end;

        m := (edge.O['a'].F['y']-edge.O['b'].F['y']) / (edge.O['a'].F['x']-edge.O['b'].F['x']);
        // t = y1 - m*x1
        t := edge.O['a'].F['y'] - m*edge.O['a'].F['x'];

        temp := m*x + t;
        Result := floatEquals(temp, y);
    end;

    function isPointOnEdge(edge, point: TJsonObject): boolean;
    begin
        Result := isPointOnEdgeFloats(edge, point.F['x'], point.F['y']);
    end;

    function intersectParallelEdges(edge1, edge2: TJsonObject): TJsonObject;
    var
        minX, minY, maxX, maxY: float;
        canContinue, e1aOnEdge, e1bOnEdge, e2aOnEdge, e2bOnEdge: boolean;
        relevantEdge1Point, relevantEdge2Point: TJsonObject;
    begin
        Result := nil;

        e1aOnEdge := isPointOnEdge(edge2, edge1.O['a']);
        e1bOnEdge := isPointOnEdge(edge2, edge1.O['b']);
        e2aOnEdge := isPointOnEdge(edge1, edge2.O['a']);
        e2bOnEdge := isPointOnEdge(edge1, edge2.O['b']);

        if ((not e1aOnEdge) and (not e1bOnEdge)) or ((not e2aOnEdge) and (not e2bOnEdge)) then exit; // they don't touch

        if (e1aOnEdge and e1bOnEdge) or (e2aOnEdge and e2bOnEdge) then exit; // one is completely inside the other

        if(e1aOnEdge) then begin
            relevantEdge1Point := edge1.O['a'];
        end else begin
            relevantEdge1Point := edge1.O['b'];
        end;

        if(e2aOnEdge) then begin
            relevantEdge2Point := edge2.O['a'];
        end else begin
            relevantEdge2Point := edge2.O['b'];
        end;

        Result := TJsonObject.create;
        Result.F['x'] := (relevantEdge1Point.F['x']+relevantEdge2Point.F['x'])/2.0;
        Result.F['y'] := (relevantEdge1Point.F['y']+relevantEdge2Point.F['y'])/2.0;
        exit;

        canContinue := isPointOnEdge(edge1, edge2.O['a']) or isPointOnEdge(edge1, edge2.O['b']) ;

        {
        // do the two actually overlap?
        // let's check if any of edge2 are within edge1
        // check if point a of edge2 is on edge1
        if (edge1.O['a'].F['x'] <= edge2.O['a'].F['x']) and (edge2.O['a'].F['x'] <= edge1.O['b'].F['x']) then begin
            if (edge1.O['a'].F['y'] <= edge2.O['a'].F['y']) and (edge2.O['a'].F['y'] <= edge1.O['b'].F['y']) then begin
                canContinue := true;
            end;
        end;

        if(not canContinue) then begin
            // check if point b of edge2 is on edge1
            if (edge1.O['a'].F['x'] <= edge2.O['b'].F['x']) and (edge2.O['b'].F['x'] <= edge1.O['b'].F['x']) then begin
                if (edge1.O['a'].F['y'] <= edge2.O['b'].F['y']) and (edge2.O['b'].F['y'] <= edge1.O['b'].F['y']) then begin
                    canContinue := true;
                end;
            end;
        end;
        }
        if(not canContinue) then begin
            exit;
        end;



        minX := getMinOf4(edge1.O['a'].F['x'], edge1.O['b'].F['x'], edge2.O['a'].F['x'], edge2.O['b'].F['x']);
        minY := getMinOf4(edge1.O['a'].F['y'], edge1.O['b'].F['y'], edge2.O['a'].F['y'], edge2.O['b'].F['y']);

        maxX := getMaxOf4(edge1.O['a'].F['x'], edge1.O['b'].F['x'], edge2.O['a'].F['x'], edge2.O['b'].F['x']);
        maxY := getMaxOf4(edge1.O['a'].F['y'], edge1.O['b'].F['y'], edge2.O['a'].F['y'], edge2.O['b'].F['y']);


        AddMessage('minX='+FloatToSTr(minX)+', minY='+FloatToSTr(minY)+', maxX='+FloatToStr(maxX)+', maxY='+FloatToStr(maxY));
        Result := TJsonObject.create;
        Result.F['x'] := (maxX+minX)/2.0;
        Result.F['y'] := (maxY+minY)/2.0;
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
        y1 := edge1.O['a'].F['y'];

        x2 := edge1.O['b'].F['x'];
        y2 := edge1.O['b'].F['y'];


        x3 := edge2.O['a'].F['x'];
        y3 := edge2.O['a'].F['y'];

        x4 := edge2.O['b'].F['x'];
        y4 := edge2.O['b'].F['y'];


        // s*(x2-x1)-t*(x4-x3)=x3-x1
        // s*(y2-y1)-t*(y4-y3)=y3-y1

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
        if floatEquals(temp1, 0) then begin
            if(debugMode) then begin
                AddMessage('Doing the intersect parallel edges hack');
            end;
            // in this case, we seem to have the same incline and be overlapping
            Result := intersectParallelEdges(edge1, edge2);
            //AddMessage('Bork intersect: Edge1='+edge1.toString()+', Edge2='+edge2.toString()+', point='+Result.toString());
            exit;
        end;

        t := ( (y3-y1)*(x2-x1) - (x3-x1)*(y2-y1) ) / temp1;

        if floatEquals(x2-x1, 0) then begin
            // means edge1 is a vertical (in Y direction) line
            if floatEquals(y2-y1, 0) then begin
                // this shouldn't happen, this would mean, edge1 has zero length
                AddMessage('ERROR: zero-length edge recieved');
                exit;
            end;
            s := (y3-y1 + t*(y4-y3)) / (y2-y1);
        end else begin
            s := (x3-x1 + t*(x4-x3)) / (x2-x1);
        end;


        // now s and t must be between 0 and 1
        if (0.0 - EPSILON <= s) and (s <= 1.0 + EPSILON) then begin
            if (0.0 - EPSILON <= t) and (t <= 1.0 + EPSILON) then begin
                // yes
                xResult := (x1 + s*(x2-x1));
                yResult := (y1 + s*(y2-y1));
                Result := TJsonObject.create;
                Result.F['x'] := xResult;
                Result.F['y'] := yResult;

                //failsafe:
                if(not isPointOnEdge(edge1, Result)) then begin
                    AddMessage('ERRROR: intersect point is not on edge1!');
                end;

                if(not isPointOnEdge(edge2, Result)) then begin
                    AddMessage('ERRROR: intersect point is not on edge2!');
                end;
            end;
        end;
    end;

    {
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
    }

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
                if(debugMode) then begin
                    AddMessage('found intersection between edge '+testEdge.toString()+' and '+curEdge.toString()+': '+isPoint.toString());
                end;
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
        // edgecase (huehue): points are identical
        if(pointsEqual(p1, p2)) then exit;
        //x1, y1, z1, x2, y2, z2: float
        newEdge := edgeArray.addObject();
        newEdge.O['a'].F['x'] := p1.F['x'];
        newEdge.O['a'].F['y'] := p1.F['y'];
        newEdge.O['a'].F['z'] := p1.F['z'];

        newEdge.O['b'].F['x'] := p2.F['x'];
        newEdge.O['b'].F['y'] := p2.F['y'];
        newEdge.O['b'].F['z'] := p2.F['z'];

        if(debugMode) then begin
            AddMessage('appended edge '+newEdge.toString());
        end;
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

    function getSlope(point1, point2: TJsonObject): string;
    var
        m: float;
    begin
        if(point1.F['x'] = point2.F['x']) then begin
            // vertical line
            // AddMessage('vertical line');
            Result := 'infinity';
            exit;
        end;

        if(point1.F['y'] = point2.F['y']) then begin
            //AddMessage('horizontal line');
            // horizontal line
            Result := '0';
            exit;
        end;

        m := (point1.F['y']-point2.F['y']) / (point1.F['x']-point2.F['x']);
        Result := FormatFloat('0.0000', m);
    end;

    function pointsEqual(p1, p2: TJsonObject): boolean;
    begin
        Result := (floatEquals(p2.F['x'], p1.F['x']) and floatEquals(p2.F['y'], p1.F['y']));
    end;

    function fixIntersectPoints(points: TJsonArray): TJsonArray;
    var
        i, lastI: integer;
        prevPoint, curPoint: TJsonObject;
        curX, curY, prevX, prevY: float;
        prevSlope, curSlope: string;
        tempResult: TJsonArray;
    begin
        tempResult := TJsonArray.create;
        Result := TJsonArray.create;
        // first, remove identical
        prevPoint := points.O[0];
        appendObjectToArray(tempResult, points.O[0]);
        for i:=1 to (points.count-1) do begin
            curPoint := points.O[i];
            if (not pointsEqual(prevPoint, curPoint)) then begin
                // add
                appendObjectToArray(tempResult, points.O[i]);
            end else begin
                AddMessage('skipping point 2');
            end;
            prevPoint := curPoint;
        end;

        prevSlope := getSlope(tempResult.O[0], tempResult.O[1]);

        // prepend first
        curPoint := Result.addObject();
        curPoint.F['x'] := tempResult.O[0].F['x'];
        curPoint.F['y'] := tempResult.O[0].F['y'];

        for i:=1 to (tempResult.count-2) do begin
            curSlope := getSlope(tempResult.O[i], tempResult.O[i+1]);
            AddMessage('checking points #'+IntToStr(i)+' and #'+IntTOStr(i+1)+' slope '+curSlope);

                // if same sl
                if(curSlope <> prevSlope) then begin
                    // add point #i, otherwise skip it
                    curPoint := Result.addObject();
                    curPoint.F['x'] := tempResult.O[i].F['x'];
                    curPoint.F['y'] := tempResult.O[i].F['y'];
                end else begin
                    AddMessage('skipping a point');
                end;
                prevSlope := curSlope;

        end;
        // add last
        lastI := tempResult.count-1;
        curPoint := Result.addObject();
        curPoint.F['x'] := tempResult.O[lastI].F['x'];
        curPoint.F['y'] := tempResult.O[lastI].F['y'];

        tempResult.free();
    end;

    function arePointsEqual(p1, p2: TJsonObject): boolean;
    begin
        Result :=  (floatEquals(p1.F['x'], p2.F['x']) and floatEquals(p1.F['y'], p2.F['y']));
    end;

    // run this twice, with different order of arguments, to generate two halves, then merge
    // this merges poly2 into poly1
    procedure mergePolygonsHalf(poly1, poly2, outPoly: TJsonObject);
    var
        i: integer;
        curEdge, isPoint1, isPoint2, isPoint3, newEdge: TJsonObject;
        pointAin, pointBin: boolean;
        edgesArray, intersectPoints, sortedIntersectPoints, tmp: TJsonArray;
    begin
        if(debugMode) then begin
            AddMessage('mergePolygonsHalf BEGIN');
        end;
        // outArray := TJsonArray.create();
        edgesArray := outPoly.A['edges'];
        // add poly2 to poly1
        if(debugMode) then begin
            AddMessage('Adding to poly = '+poly1.toString());
        end;
        // let's iterate poly2's edges
        for i:=0 to poly2.A['edges'].count - 1 do begin
            curEdge := poly2.A['edges'].O[i];
            if(debugMode) then begin
                AddMessage('Checking edge #'+IntToStr(i)+' = '+curEdge.toString());
            end;

            intersectPoints := intersectPolyWithEdgeMulti(poly1, curEdge);

            // the edge belongs to poly2. we are testing whenever the end points of the edge are within poly1 or not.
            //
            pointAin := isPointInPolygonEdges(poly1.A['edges'], curEdge.O['a'].F['x'], curEdge.O['a'].F['y']);
            pointBin := isPointInPolygonEdges(poly1.A['edges'], curEdge.O['b'].F['x'], curEdge.O['b'].F['y']);


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
                            // this can happen, sigh. probably if there are parallel lines
                            // let's try adding it
                            AddMessage('ANOMALY: Edge doesn''t intersect, but one of it''s points is inside, and the other is outside');
                            haveAnomalies := true;
                            //appendObjectToArray(edgesArray, curEdge);
                            //AddMessage('!!!ANOMALY: Edge doesn''t intersect, but points are halfway? This makes no sense');
                        end;
                    end;
                1:  begin
                        if(debugMode) then begin
                            AddMessage('One Intersect');
                        end;
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
                            // so apparently, something can be wrong here
                            appendEdge(edgesArray, curEdge.O['a'], isPoint1);
                        end else begin
                            {
                            new stuff:
                            - if both points are outside, the sole intersection must lie right on the edge. 
                                I think this means, we should just add the entire edge.
                            }
                            if((not pointAin) and (not pointBin)) then begin
                                if(debugMode) then begin
                                    AddMessage(' A--B');
                                end;
                                appendEdge(edgesArray, curEdge.O['a'], curEdge.O['b']);
                            end else if (pointAin and pointBin) then begin
                                if(debugMode) then begin
                                    AddMessage(' ...nothing?');
                                end;
                                AddMessage(' MAYBE an anomaly? We seem to have one intersection, but both segment ends seem to be inside the polygon. This should mean, it'' safe to leave the segment out entirely, but if you see this message and your border turns out weirdly, please report this to Pra.');
                            end else begin
                                {
                                // this can mean that the intersect point lies right on the edge, but what does THAT mean?
                                if(arePointsEqual(curEdge.O['a'], intersectPoints.O[0])) then begin
                                    // add the edge as is
                                    AddMessage(' A--B');
                                    appendEdge(edgesArray, curEdge.O['a'], curEdge.O['b']);
                                end else if(arePointsEqual(curEdge.O['b'], intersectPoints.O[0])) then begin
                                    // add the edge as is
                                    AddMessage(' A--B');
                                    appendEdge(edgesArray, curEdge.O['a'], curEdge.O['b']);
                                end else begin
                                    AddMessage('ANOMALY: one intersect, but pointAin='+BoolToStr(pointAin)+' pointBin='+BoolToStr(pointBin)+'. Skipping');
                                    AddMessage('Point A: '+curEdge.O['a'].toString());
                                    AddMessage('Point B: '+curEdge.O['b'].toString());
                                    AddMessage('Intersect: '+intersectPoints.O[0].toString());
                                end;
                                }
                                AddMessage('ANOMALY: one intersect, but pointAin='+BoolToStr(pointAin)+' pointBin='+BoolToStr(pointBin));
                                AddMessage('This probably means that your build areas are aligned too much, you can try rotating, extending, or moving one of them by a little bit.');
                                haveAnomalies := true;
                            end;
                        end;
                    end;
                else begin
                        if(debugMode) then begin
                            AddMessage('N intersects, n = '+IntToStr(intersectPoints.count));
                        end;

                        //  - sort the intersect points by their distance to A
                        //  - if A is outside, prepend it
                        //  - if B is outside, append it
                        //  - each pair of points (0..1, 2..3, 4..5, ..) is an edge. In theory, this should always be an even nr of points

                        sortedIntersectPoints := sortPointsByDistance(curEdge.O['a'], intersectPoints);
                        //AddMessage('Sorted list: '+sortedIntersectPoints.toString());
                        if (not pointAin) then begin
                            prependObjectToArray(sortedIntersectPoints, curEdge.O['a']);
                        end;
                        if (not pointBin) then begin
                            appendObjectToArray(sortedIntersectPoints, curEdge.O['b']);
                        end;
                        if ((sortedIntersectPoints.count mod 2) <> 0) then begin
                            //AddMessage('ERROR: sortedIntersectPoints ended up at an odd length!');
                            //AddMessage(sortedIntersectPoints.toString());
                            // trying to do some extras
                            tmp := fixIntersectPoints(sortedIntersectPoints);
                            sortedIntersectPoints.free();
                            sortedIntersectPoints := tmp;
                            //exit;
                        end;

                        if ((sortedIntersectPoints.count mod 2) <> 0) then begin
                            AddMessage('ERROR: sortedIntersectPoints ended up at an odd length!');
                            haveAnomalies := true;
                            exit;
                        end;


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


                        sortedIntersectPoints.free();
                    end;
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

    function getBoxPoints(boxPos, boxRot, boxSize: TJsonObject; includeZ: boolean): TJsonArray;
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

        // point 2:
        point2rel := newVector(
            boxSize.F['x'] / 2,
            boxSize.F['y'] / 2 * -1,
            0
        );

        // point 3:
        point3rel := newVector(
            boxSize.F['x'] / 2 * -1,
            boxSize.F['y'] / 2 * -1,
            0
        );

        // point 4:
        point4rel := newVector(
            boxSize.F['x'] / 2 * -1,
            boxSize.F['y'] / 2,
            0
        );

        if(includeZ) then begin
            point1rel.F['z'] := boxSize.F['z'] / -2;
            point2rel.F['z'] := boxSize.F['z'] / -2;
            point3rel.F['z'] := boxSize.F['z'] / -2;
            point4rel.F['z'] := boxSize.F['z'] / -2;
        end;

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

    function addBoxToCluster(boxPoints: TJsonArray; clusterPolygon: TJsonObject): TJsonObject;
    var
        boxPos, boxRot, boxSize, curPoint: TJsonObject;
        boxPosAdj, curPoly, newPoly: TJsonObject;

        points, edges: TJsonArray;
        boxDownSize: float;
    begin


        edges := getEdgesFromPoints(boxPoints);


        curPoly := TJsonObject.create;
        curPoly.A['edges'] := edges;
        //AddMessage('curPoly='+curPoly.toString());

        if(clusterPolygon = nil) then begin
            Result := curPoly;
        end else if(clusterPolygon.count = 0) then begin
            // clusterPolygon.free();
            Result := curPoly;
        end else begin
            newPoly := mergePolygons(curPoly, clusterPolygon);
            //AddMessage('merged newPoly='+newPoly.toString());
            curPoly.free();
            //clusterPolygon.free();
            Result := newPoly;
        end;

        //points.free();
        //boxSize.free();
        //boxRot.free();
        //boxPos.free();
        //boxPosAdj.free();
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



    function addTerrainHeightForPolygon(curPolygon: TJsonObject; workShop, worldSpace: IInterface): TJsonObject;
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
        for i:=0 to curPolygon.A['edges'].count-1 do begin
            curEdge := curPolygon.A['edges'].O[i];

            point1 := cloneJsonObject(curEdge.O['a']);
            point2 := cloneJsonObject(curEdge.O['b']);

            point1.F['z'] := getTerrainHeight(wsX + point1.F['x'], wsY + point1.F['y'], worldSpace) - wsZ;
            point2.F['z'] := getTerrainHeight(wsX + point2.F['x'], wsY + point2.F['y'], worldSpace) - wsZ;

            // gather data for this
            if (flattenBottomEdge) then begin
                if((point1.F['z']-borderDownHeight) < borderMinHeight) then begin
                    borderMinHeight := point1.F['z'] - borderDownHeight;
                end;
                if((point2.F['z']-borderDownHeight) < borderMinHeight) then begin
                    borderMinHeight := point2.F['z'] - borderDownHeight;
                end;
            end;

            addIntermediatePoints(worldSpace, point1, point2, newPolygon.A['edges'], wsX, wsY, wsZ);

            point1.free();
            point2.free();
        end;

        Result := newPolygon;

        //prevPolygon := currentPolygon;
        //currentPolygon := newPolygon;
        //prevPolygon.free();
    end;

    function ShowSaveFileDialogWithStartPath(title: string; filter: string; startPath: string): string;
    var
        objFile: TSaveDialog;
    begin
        objFile := CreateSaveFileDialog(title, filter, startPath);
        Result := '';
        try
            if objFile.Execute then begin
                Result := objFile.FileName;
            end;
        finally
            objFile.free;
        end;
    end;


    procedure outputBrowseHandler(Sender: TObject);
	var
		inputPath: TEdit;
		pathStr: string;
        curPath: string;
	begin
		inputPath := TLabeledEdit(sender.parent.FindComponent('editOutput'));
        curPath := ExtractFilePath(inputPath.text); // attempt to start in current dir
		pathStr := ShowSaveFileDialogWithStartPath('Save Output As', 'NIF Files|*.nif|All Files|*.*', curPath);
		if(pathStr <> '') then begin
            if(not strEndsWithCI(pathStr, '.nif')) then begin
                pathStr := pathStr + '.nif';
            end;
			inputPath.Text := pathStr;
		end;
	end;


    function getWorkshopLocation(wsRef: IInterface): IInterface;
    var
        wsX, wsY: float;
        cellX, cellY: integer;
        worldSpace, cell: IInterface;
    begin
        Result := pathLinksTo(wsRef, 'XLRL');
        if(assigned(Result)) then begin
           exit;
        end;

        worldSpace := getParentWorldSpace(wsRef);
        if(not assigned(worldSpace)) then exit;

        wsX := GetElementNativeValues(wsRef, 'DATA\Position\X');
        wsY := GetElementNativeValues(wsRef, 'DATA\Position\Y');

        cellX := getGridCoord(wsX);
        cellY := getGridCoord(wsY);

        cell := getWorldspaceCell(worldSpace, cellX, cellY);

        Result := pathLinksTo(cell, 'XLCN');
    end;

    function getWorkshopDescription(wsRef: IInterface): string;
    var
        location: IInterface;
    begin
        location := getWorkshopLocation(wsRef);
        if(assigned(location)) then begin
            Result := ShortName(wsRef) + ' at ' + getElementEditValues(location, 'FULL');
            exit;
        end;
        Result := ShortName(wsRef);
    end;

    function generatePathForWorkshop(wsRef: IInterface): string;
    var
        location: IInterface;
        baseStr: string;
    begin
        Result := '';
        baseStr := EditorID(wsRef);
        if(baseStr = '') then begin
            location := getWorkshopLocation(wsRef);
            if(not assigned(location)) then exit;

            baseStr := cleanStringForEditorID(getElementEditValues(location, 'FULL'));
        end;

        if(baseStr = '') then exit;

        // DataPath includes \
        Result := DataPath + autoPathBase + baseStr+ '.nif';
    end;

    procedure autoFillOutputPath(sender: TObject);
    var
        cbAutoOutputFile: TCheckBox;
        path: string;
        editOutput: TLabeledEdit;
    begin
		editOutput := TLabeledEdit(sender.parent.FindComponent('editOutput'));
        if(editOutput.Text <> '') then exit;

		cbAutoOutputFile := TCheckBox(sender.parent.FindComponent('cbAutoOutputFile'));
        if(cbAutoOutputFile.checked) then begin
            path := generatePathForWorkshop(currentGuiWorkshopRef);
            editOutput.Text := path;
        end;
    end;


    procedure outputFileChangeHandler(sender: TObject);
    var
        cbAutoOutputFile: TCheckBox;
        path: string;
        editOutput: TLabeledEdit;
        btnOk: TButton;
    begin
		editOutput := TLabeledEdit(sender.parent.FindComponent('editOutput'));
		btnOk := TButton(sender.parent.FindComponent('btnOk'));

        btnOk.enabled := (trim(editOutput.Text) <> '');
    end;


    function showGui(wsRef: IInterface): boolean;
    var
        frm: TForm;
        btnOk, btnCancel, btnBrowse: TButton;
		editOutput, editHeightTop, editHeightBottom, editPrecision, editTolerance, editHeightHelperBoxEdid: TLabeledEdit;
        resultCode: cardinal;
        cacheLabel: TLabel;
        cbExtendToBottom, cbUseWaterHeight, cbSoftBottomEdge, cbFlattenBottomEdge, cbDoFullPrec, cbAutoOutputFile: TCheckBox;
    begin
        currentGuiWorkshopRef := wsRef;
        Result := false;
        frm := CreateDialog('Generate Workshop Border', 450, 380);
        CreateLabel(frm, 10, 10, 'Processing: '+getWorkshopDescription(wsRef));

        editOutput := CreateLabelledInput(frm, 10, 50, 380, 50, 'Output File:', '');
        editOutput.Name := 'editOutput';
        editOutput.Text := '';

        btnBrowse := CreateButton(frm, 400, 48, '...');
        btnBrowse.onclick := outputBrowseHandler;

        if(autoOutputFile) then begin
            editOutput.Text := generatePathForWorkshop(wsRef);
        end;

        {borderHeight: float;
        borderPrecision: float;
        borderDownHeight: float;}

        editHeightTop := CreateLabelledInput(frm, 10, 100, 160, 50, 'Border Height', FloatToStr(borderHeight));
        editHeightBottom := CreateLabelledInput(frm, 200, 100, 160, 50, 'Border Depth', FloatToStr(borderDownHeight));


        editPrecision := CreateLabelledInput(frm, 10, 140, 160, 50, 'Step Size', FloatToStr(borderPrecision));
        editTolerance := CreateLabelledInput(frm, 200, 140, 160, 50, 'Height Tolerance', FloatToStr(heightTolerance));

        cbExtendToBottom := CreateCheckbox(frm, 10, 180, 'Extend to Bottom of Build Area');
        cbExtendToBottom.checked := extendToBottom;

        cbUseWaterHeight := CreateCheckbox(frm, 200, 180, 'Use Cell Water Height');
        cbUseWaterHeight.checked := useWaterHeight;

        cbSoftBottomEdge := CreateCheckbox(frm, 10, 200, 'Soft Bottom Edge');
        cbSoftBottomEdge.checked := softBottomEdge;

        cbFlattenBottomEdge := CreateCheckbox(frm, 200, 200, 'Flatten Bottom Edge');
        cbFlattenBottomEdge.checked := flattenBottomEdge;

        cbDoFullPrec := CreateCheckbox(frm, 10, 220, 'Full Precision');
        cbDoFullPrec.checked := doFullPrec;

        cbAutoOutputFile := CreateCheckbox(frm, 200, 220, 'Auto Output File');
        cbAutoOutputFile.checked := autoOutputFile;
        cbAutoOutputFile.Name := 'cbAutoOutputFile';
        cbAutoOutputFile.onclick := autoFillOutputPath;


        editHeightHelperBoxEdid := CreateLabelledInput(frm, 10, 260, 380, 50, 'Height Override Marker (Editor ID)', heightHelperBoxEdid);



        btnOk     := CreateButton(frm, 130, 300, '  OK  ');
        btnCancel := CreateButton(frm, 210, 300, 'Cancel');

        btnOk.Default := true;
        btnOk.Name := 'btnOk';
        btnOk.ModalResult := mrYes;

        btnCancel.ModalResult := mrCancel;


        outputFileChangeHandler(editOutput);
        editOutput.onchange := outputFileChangeHandler;

        resultCode := frm.ShowModal();
        if(resultCode = mrYes) then begin
            borderHeight := StrToFloat(editHeightTop.Text);
            borderPrecision := StrToFloat(editPrecision.Text);
            borderDownHeight := StrToFloat(editHeightBottom.Text);
            heightTolerance := StrToFloat(editTolerance.Text);

            extendToBottom := cbExtendToBottom.checked;
            useWaterHeight := cbUseWaterHeight.checked;
            softBottomEdge := cbSoftBottomEdge.checked;
            flattenBottomEdge := cbFlattenBottomEdge.checked;
            doFullPrec := cbDoFullPrec.checked;
            heightHelperBoxEdid := trim(editHeightHelperBoxEdid.text);

            autoOutputFile := cbAutoOutputFile.checked;

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


    function addHelperBoxRef(ref: IInterface): boolean;
    var
        ws, xprm: IInterface;
        wsKey, refKey: string;
        watEdges, posVector, rotVector, boxSize, newEntry, newPoint: TJsonObject;
        boxDownSize: float;
        wsEntry, points: TJsonArray;
        i: integer;
    begin
        //AddMessage('trying '+FullPath(ref));
        Result := false;
        ws := getParentWorldSpace(ref);
        if (not assigned(ws)) then begin
            //AddMessage('no ws');
            exit;
        end;

        xprm := ElementByPath(ref, 'XPRM');
        if (not assigned(xprm)) then begin
            //AddMessage('no xprm');
            exit;
        end;

        if (getElementEditValues(xprm, 'Type') <> 'Box') then begin
            //AddMessage('not box');
            exit;
        end;
        // addHelperBoxRef

        posVector := getPositionVector(ref, 'DATA');
        rotVector := getRotationVector(ref, 'DATA');
        boxSize := getSizeVector(ref);

        if(boxSize.F['x'] = 0.0) or (boxSize.F['y'] = 0.0) or (boxSize.F['z'] = 0.0) then begin
            //AddMessage('no size');
            exit;
        end;

        wsKey := FormToAbsStr(ws);

        points := getBoxPoints(posVector, rotVector, boxSize, true);
        // I think we should reorder the points, so that they are in the NO SO SW NW position

        wsEntry := helperBoxes.O['worldspaces'].A[wsKey];
        newEntry := wsEntry.addObject();

        newEntry.S['ref'] := FormToAbsStr(ref);
        for i:=0 to points.count-1 do begin
            newPoint := newEntry.A['points'].addObject();
            newPoint.F['x'] := points.O[i].O['pos'].F['x'];
            newPoint.F['y'] := points.O[i].O['pos'].F['y'];
            newPoint.F['z'] := points.O[i].O['pos'].F['z'];

            // newEntry.A['points'] := points;
        end;

        watEdges := createPolygonData(points);
        newEntry.A['edges'] := watEdges.A['edges'];
        // AddMessage('newEntry = '+newEntry.toString());

        posVector.free();
        rotVector.free();
        points.free();
        boxSize.free();

        Result := true;
    end;

    procedure loadHelperBoxes();
    var
        baseObj, curRef: IInterface;
        i, numBoxes: integer;
    begin
        if(heightHelperBoxEdid = '') then exit;

        if(helperBoxes.S['prevEdid'] = heightHelperBoxEdid) then begin
            // still valid
            exit;
        end;

        helperBoxes.clear();
        helperBoxes.S['prevEdid'] := heightHelperBoxEdid;

        baseObj := FindObjectByEdidAndSignature(heightHelperBoxEdid, 'ACTI');
        if(not assigned(baseObj)) then begin
            AddMessage('Didn''t find any activator with EditorID '+heightHelperBoxEdid+', skipping helper boxes.');
            exit;
        end;
        AddMessage('Loading helper boxes with EditorID '+heightHelperBoxEdid);

        for i := 0 to ReferencedByCount(baseObj)-1 do begin
            curRef := WinningOverrideOrSelf(ReferencedByIndex(baseObj, i));

            if(Signature(curRef) <> 'REFR') then continue;

            if(isSameForm(pathLinksTo(curRef, 'NAME'), baseObj)) then begin
                if(addHelperBoxRef(curRef)) then begin
                    numBoxes := numBoxes + 1;
                end;
            end;
        end;
        AddMessage('Found '+IntToStr(numBoxes)+' helper boxes');
        if(debugMode) then begin
            AddMessage('Helper Box Data: '+helperBoxes.toString());
        end;
    end;

    function getParentWorldSpace(ref: IInterface): IInterface;
    var
        wsCell: IInterface;
    begin
        Result := nil;
        wsCell := pathLinksTo(ref, 'CELL');
        if(isCellInterior(wsCell)) then exit;
        // find the worldspace of this wsRef
        Result := pathLinksTo(wsCell, 'Worldspace');
    end;

    function prepareBoxPoint(boxRef: IInterface): TJsonArray;
    var
        boxPos, boxRot, boxSize, curPoint: TJsonObject;
        boxPosAdj, curPoly, newPoly: TJsonObject;

        points, edges: TJsonArray;
        boxDownSize: float;
    begin
        Result := nil;
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


        boxDownSize := boxPosAdj.F['z'] - ( boxSize.F['z'] / 2);

        if(boxDownSize < wsMinHeight) then begin
            wsMinHeight := boxDownSize;
        end;

        boxPosAdj.F['z'] := 0;

        Result := getBoxPoints(boxPosAdj, boxRot, boxSize, false);

        boxSize.free();
        boxRot.free();
        boxPos.free();
        boxPosAdj.free();
    end;

    function doPolysIntersect(points1, points2: TJsonArray): boolean;
    var
        edges1: TJsonArray;
        point: TJsonObject;
        i: integer;
    begin
        edges1 := getEdgesFromPoints(points1);

        for i:=0 to points2.count-1 do begin
            point := points2.O[i];
            if(isPointInPolygonEdges(edges1, point.O['pos'].F['x'], point.O['pos'].F['y'])) then begin
                Result := true;
                exit;
            end;
            {
            curEdge.O['a'].F['x'] := point1.O['pos'].F['x'];
            curEdge.O['a'].F['y'] := point1.O['pos'].F['y'];
            }
        end;
        Result := false;
    end;

    function doesPolyIntersectWithList(listOfPoints, points: TJsonArray): boolean;
    var
        i: integer;
    begin
        for i:=0 to listOfPoints.count - 1 do begin
            if(doPolysIntersect(listOfPoints.A[i], points)) then begin
                Result := true;
                exit;
            end;

            if(doPolysIntersect(points, listOfPoints.A[i])) then begin
                Result := true;
                exit;
            end;
        end;
        Result := false;
    end;

    procedure ensureArrayLength(arr: TJsonArray; targetLength: integer);
    var
        i: integer;
    begin
        while(arr.count < targetLength) do begin
            arr.addArray();
        end;
    end;

    {
        Do three things for the boxRefs:
        1.) transform them into arrays of points (return values of getBoxPoints)
        2.) sort these in such a way, that each box besides the first has at least one point within any of the previous boxes
        3.) return an array of arrays of arrays of points:
            first layer: one entry for each disjunct border
            second layer: one entry for each box
            third layer: one entry for each point (should be exactly 4)
    }
    function prepareBoxPoints(boxRefs: TList): TJsonArray;
    var
        i, clusterNr: integer;
        curBox, firstBox: TJsonArray;
        boxesUnsorted, currentCluster: TJsonArray;
        done, changesThisIteration: boolean;
    begin
        // Result: list of disjunct shapes
        Result := TJsonArray.create();

        if(boxRefs.count = 0) then begin
            exit;
        end;

        boxesUnsorted := TJsonArray.create;

        // DEBUG: try one less
        for i:=0 to boxRefs.count-1 do begin
            appendArrayToArray(boxesUnsorted, prepareBoxPoint(ObjectToElement(boxRefs[i])));
        end;

        clusterNr := 0;
        while(boxesUnsorted.count > 0) do begin
            firstBox := removeFromArray(boxesUnsorted, 0);

            // - remove the first element from boxesUnsorted, put it into the current cluster nr
            ensureArrayLength(Result, clusterNr + 1);
            currentCluster := Result.A[clusterNr];
            //AddMessage('trying to append to '+currentCluster.toJson()+' this '+firstBox.toJson());
            appendArrayToArray(currentCluster, firstBox);
            //AddMessage('while with the done');

            done := false;
            while(not done) do begin

                // - keep iterating the rest
                //  - if during iteration the current box intersects with any of the boxes in current cluster, remove it from boxesUnsorted and append it to current cluster.
                //  - if one iteration runs through without finding any intersection, increment clusterNr by 1 and continue with the outer while loop.
                changesThisIteration := false;

                for i:=0 to boxesUnsorted.count-1 do begin
                    curBox := boxesUnsorted.A[i];

                    if(doesPolyIntersectWithList(currentCluster, curBox)) then begin
                        changesThisIteration := true;
                        curBox := removeFromArray(boxesUnsorted, i);
                        appendArrayToArray(currentCluster, curBox);
                        break;
                    end;
                end;

                if(not changesThisIteration) then begin
                    clusterNr := clusterNr + 1;
                    done := true;
                end;
            end;
        end;

        boxesUnsorted.free();

    end;

    procedure processWorkshop(wsRef: IInterface);
    var
        boxes: TList;
        i, j: integer;
        watTest, worldSpace, wsCell: IInterface;
        boxClusters: TJsonArray;
        curInputCluster, curInputBox: TJsonArray;

        curPoly, newPoly: TJsonObject;
        clusterPolygons, polygonsWithHeight: TJsonArray;
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
        if(boxes.count = 0) then begin
            if(debugMode) then begin
                AddMessage('Found no building area boxes for '+Name(wsRef)+', nothing to do.');
            end;
            boxes.free();
            exit;
        end;
        AddMessage('Processing Workshop: ' + FullPath(wsRef)+' with '+IntToStr(boxes.count)+' build area boxes');
        if (not showGui(wsRef)) then begin
            exit;
        end;

        wsOrigin := getPositionVector(wsRef, 'DATA');
        wsMinHeight := wsOrigin.F['z'] + 900000;
        borderMinHeight := wsMinHeight;

        //currentPolygon := TJsonObject.create;

        // Sort the boxes, so that each box has at least one point within any of the previous ones
        
        AddMessage('Sorting build area boxes');
        boxClusters := prepareBoxPoints(boxes);

        //AddMessage(boxClusters.toJson(boxClusters));
        // boxClusters.saveToFile('foobar.json', false);
        if(debugMode) then begin
            writeSvgClusters(boxClusters, 'clusters_unmerged.svg');
        end;

        clusterPolygons := TJsonArray.create();

        // now try to build N polys
        AddMessage('Building '+IntToStr(boxClusters.count)+' 2D polygons');
        for i:=0 to boxClusters.count-1 do begin
            AddMessage('Building polygon #'+IntToStr(i+1));
            curInputCluster := boxClusters.A[i];
            curPoly := nil;

            //addBoxToCluster
            for j:=0 to curInputCluster.count-1 do begin
                curInputBox := curInputCluster.A[j];
                newPoly := addBoxToCluster(curInputBox, curPoly);
                if(curPoly <> nil) then begin
                    curPoly.free();
                end;
                curPoly := newPoly;
            end;

            appendObjectToArray(clusterPolygons, curPoly);

            if(debugMode) then begin
                writeSvg(curPoly, 'output_'+IntToStr(i)+'.svg');
            end;
            curPoly.free();
        end;

        if(haveAnomalies) then begin
            AddMessage('Anomalies were found while trying to generate the border.');
            AddMessage('You can try to tweak your build area boxes somewhat, make sure no points align exactly with other points or edges.');
            AddMessage('No mesh will be generated for Workshop '+Name(wsRef));
            wsOrigin.free();
            boxes.free();
            clusterPolygons.free();
            exit;
        end;

        polygonsWithHeight := TJsonArray.create;

        AddMessage('Loading helper markers');
        // load helper boxes
        loadHelperBoxes();


        AddMessage('Loading terrain height');
        // add terrain height
        for i:=0 to clusterPolygons.count-1 do begin
            curPoly := clusterPolygons.O[i];
            newPoly := addTerrainHeightForPolygon(curPoly, wsRef, worldSpace);

            appendObjectToArray(polygonsWithHeight, newPoly);
        end;
        
        // write the nif
        AddMessage('Writing NIF to '+saveNifAs);
        writeNifFromPolygons(polygonsWithHeight);
        AddMessage('Finished!');

        wsOrigin.free();
        boxes.free();
        clusterPolygons.free();
        polygonsWithHeight.free();
    end;

    procedure testReference(e: IInterface);
    var
        refX, refY, terrainHeight: float;
        refWs: IInterface;
    begin
        refX := GetElementNativeValues(e, 'DATA\Position\X');
        refY := GetElementNativeValues(e, 'DATA\Position\Y');

        //refWs := pathLinksTo(pathLinksTo(e, 'CELL'), 'Worldspace');
        refWs := getParentWorldSpace(e);

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
        helperBoxes.free();
        worldspaceCellCache.free();
    end;

end.