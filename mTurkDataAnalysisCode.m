%code for the paper:
%On quality and reliability of the Amazon Mechanical Turk (mTurk) data in 2024: Challenges, pitfalls and recommendations
%Shimoni & Axelrod
function compareCatch_TwoRepeatsConsistency_MultipleFiles()

close all;

rng(123);

CONST.correl_type = 'Pearson';

CONST.ExpID = 2; %1: main text experiment; %2: replication experiment (supplementary materials)

if (1 == CONST.ExpID)
    CONST.inputFileNameArr = {'mobile_nonMasters.csv',...
                              'mobile_95Percent.csv',...                      
                              'mobile_Masters.csv'...
                              };                              
                          
    %5 catch questions, 16 questions of interest (8+8)
    %colIds in original csv file
    CONST.dataColIds = [4:24];

    %the first five are catch questions                   
    CONST.qIDs_to_compare{1} = [6:13];
    CONST.qIDs_to_compare{2} = [14:21];


elseif (2 == CONST.ExpID)
    CONST.inputFileNameArr = {'BPS_nonMasters.csv',...
                              'BPS_95Percent.csv',...
                              'BPS_Masters.csv',...                              
                              };
                          
    %5 catch questions, 20 questions of interest (10+10)
    %colIds in original csv file
    CONST.dataColIds = [4:28];

    %the first five are catch questions                   
    CONST.qIDs_to_compare{1} = [6:15];
    CONST.qIDs_to_compare{2} = [16:25];
else
    error('Unsupported CONST.ExpID');
end

                      
CONST.condNames = {'No requirements','95% approval','Master workers'};

CONST.gender_colID = 1;
CONST.age_colID = 2;
CONST.hand_colID = 3;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CONST.catchCorrectAns{1} = []; 

%columnsID are after conversion to matlab matrix
%first column is the columnID of the data
%columns 2 and 3 are the acceptable responses
%math questions
CONST.catchCorrectAns{2} = [1,7;...
                            2,1]; 

%math  + common sense q (intuitive order)                  
CONST.catchCorrectAns{3} = [1,7,NaN;...
                            2,1,NaN;...
                            3,6,7;...
                            4,6,7]; 
                  
%math  + common sense q (intuitive order) + common sense q (counterintuitive order)                
CONST.catchCorrectAns{4} = [1,7,NaN;...
                            2,1,NaN;...
                            3,6,7;...
                            4,6,7;...
                            5,1,NaN];  
                  
CONST.catchLabelsArr = {'no catch validation',...
                        'only arithmetic catches',...
                        'arithmetic and intuitive common sense catches',...
                        'arithmetic and all common sense catches'};   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% End of definitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for file_id=1:length(CONST.inputFileNameArr)
        inputFileName = CONST.inputFileNameArr{file_id};
        [reproduceCorrArrArr{file_id},statsCacthCorrectArr{file_id},...
         DEMOGRArr{file_id},...
         matrixDataArr{file_id},catchReportMatrixArr{file_id},...
         catchReportMatrix_BfrFailedCorrelFltrArr{file_id}] = ...
            ProcessFile(inputFileName, CONST, file_id);
    end
    
    %Fig. 2
    PlotBarGraphsPercentSubjects_byCatchCateg(CONST,DEMOGRArr, reproduceCorrArrArr,...
                                              catchReportMatrix_BfrFailedCorrelFltrArr);
    
    %Fig. 3
    PlotReproducibilityBarPlot_byCatchCateg(CONST,reproduceCorrArrArr);

   
    
    %Fig.4 (left)
    numberSubjectsToShow = NaN;
    PlotIdividualQResponsesBySetOverlayNoSplitRepeats(CONST, matrixDataArr, catchReportMatrixArr,...
                                                      numberSubjectsToShow);
                                                  
    %Fig.4 (right)
    numberSubjectsToShow = 30;
    PlotIdividualQResponsesBySetOverlayNoSplitRepeats(CONST, matrixDataArr, catchReportMatrixArr,...
                                                      numberSubjectsToShow);
        
    %Fig. 5
    CompareAndPlotStdAcrossQuestionsNoSplitRepeats(CONST, matrixDataArr, catchReportMatrixArr);
                                                  
    b=1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function PlotReproducibilityBarPlot_byCatchCateg(CONST,reproduceCorrArrArr)

    cfg = [];
    cfg.bCreateFigure = 0;
    cfg.bPairedTTest = 0;
    cfg.bPlotZeroLine = 0;
    cfg.bParallelcoords = 0;
    cfg.bMeanExclNaN = 0;
    cfg.bStatsShowMode = 0;
    cfg.FontSizeStruct.XAxis.FontSize = 7;
    cfg.FontSizeStruct.YAxis.FontSize = 10;
    cfg.FontSizeStruct.XLabel.FontSize = 7;
    cfg.FontSizeStruct.YLabel.FontSize = 10;
    cfg.FontSizeStruct.Title.FontSize = 8;
    cfg.IndividualPointFactor = 4;
   
    fig_ptr = figure('Position',[1 1 2000 1000]);
    num_groups = length(reproduceCorrArrArr);
    
    for catch_id=1:length(CONST.catchLabelsArr)
        dataArr = [];
        for file_id=1:num_groups
            dataArr{file_id} = reproduceCorrArrArr{file_id}{catch_id};
        end
        
        subplot(2,2,catch_id);

        bBarsOnly = 0;    
        utilsInternal_IndivScatterPlotWithBars(cfg, dataArr, CONST.condNames,...
                                               'correlation', CONST.catchLabelsArr{catch_id}, bBarsOnly);                                       
        pbaspect([1 1 1]);
        ylim([-1 1]);
        ticks = [-1:0.2:1];    
        tick_labels = arrayfun(@num2str, ticks, 'UniformOutput', false);
        yticks(ticks);
        yticklabels(tick_labels);                
    end
    sgtitle('Reliability: by catch categories');
    a = 1;

    
    
function CompareAndPlotStdAcrossQuestionsNoSplitRepeats(CONST, matrixDataArr,...
                                                        catchReportMatrixArr)

    fig_ptr = figure;   
    set(fig_ptr,'Position',[1 1 2000 1000]);

    num_catch_categ = size(catchReportMatrixArr{1},2);
    qIDs_list = [CONST.qIDs_to_compare{1} CONST.qIDs_to_compare{2}];
    for catch_id=1:num_catch_categ
        for file_id=1:length(matrixDataArr)
            matrixData = matrixDataArr{file_id};
            catchReportMatrix = catchReportMatrixArr{file_id};

            row_ids = find(1 == catchReportMatrix(:,catch_id));
            matrixData_perCatch = matrixData(row_ids,:);

            matrixWithQOfInterest = [];
            for q_id=1:length(qIDs_list)
                col_id = qIDs_list(q_id);
                matrixWithQOfInterest = [matrixWithQOfInterest matrixData_perCatch(:,col_id)];  
            end

            dataForScatterArr{file_id} = std(matrixWithQOfInterest')';                
        end

        if (num_catch_categ <= 2)
            subplot(1, num_catch_categ, catch_id);                
        else
            subplot(2, 2, catch_id);           
        end
            
        sTitle = CONST.catchLabelsArr{catch_id};

        cfg.bCreateFigure = 0;
        cfg.bPairedTTest = 0;
        cfg.bPlotZeroLine = 0;
        cfg.bParallelcoords = 0;
        cfg.bMeanExclNaN = 0;
        cfg.bStatsShowMode = 0;        
        cfg.BarFaceColor = [0.8 0.8 0.8];
        
        if (0 == cfg.bStatsShowMode)
            cfg.bFontsSpecified = 1;
            cfg.FontSizeStruct.XAxis.FontSize = 6;
            cfg.FontSizeStruct.YAxis.FontSize = 12;
            cfg.FontSizeStruct.XLabel.FontSize = 6;
            cfg.FontSizeStruct.YLabel.FontSize = 12;
            cfg.FontSizeStruct.Title.FontSize = 12;
            cfg.IndividualPointFactor = 4;
        else
            cfg.bFontsSpecified = 1;
            cfg.FontSizeStruct.XAxis.FontSize = 6;
            cfg.FontSizeStruct.YAxis.FontSize = 6;
            cfg.FontSizeStruct.XLabel.FontSize = 6;
            cfg.FontSizeStruct.YLabel.FontSize = 6;
            cfg.FontSizeStruct.Title.FontSize = 9;   
            cfg.IndividualPointFactor = 4;
        end    
        
        bBarsOnly = 0;    
        utilsInternal_IndivScatterPlotWithBars(cfg, dataForScatterArr, CONST.condNames,...
                                               'std across items', sTitle, bBarsOnly); 
                                                                      
        pbaspect([1 1 1]);                                               

        sgtitle('Comparing STD across questions');            
    end
    b = 1;
    
    
function PlotIdividualQResponsesBySetOverlayNoSplitRepeats(CONST, matrixDataArr,...
                                                           catchReportMatrixArr,...
                                                           numberSubjectsToShow)
                                                                           
    fig_ptr = figure;   
    set(fig_ptr,'Position',[1 1 2000 1000]);

    num_catch_categ = size(catchReportMatrixArr{1},2);
    qIDs_list = [CONST.qIDs_to_compare{1} CONST.qIDs_to_compare{2}];
    for catch_id=1:num_catch_categ
        for file_id=1:length(matrixDataArr)
            
            matrixData = matrixDataArr{file_id};
            catchReportMatrix = catchReportMatrixArr{file_id};

            row_ids = find(1 == catchReportMatrix(:,catch_id));
            matrixData_perCatch = matrixData(row_ids,:);
            
            num_subj_arr(file_id) = size(matrixData_perCatch,1);            

            matrixForScatter = [];
            labels = {};
            for q_id=1:length(qIDs_list)
                col_id = qIDs_list(q_id);
                matrixForScatter = [matrixForScatter matrixData_perCatch(:,col_id)];  
                labels = [labels 'q' num2str(q_id)];
            end
            dataArrForScatter{file_id} = matrixForScatter;
        end
               
        if ~isnan(numberSubjectsToShow)
            for set_id=1:length(num_subj_arr)
                if (num_subj_arr(set_id) < numberSubjectsToShow)
                    dataArrForScatter{set_id} = [];
                    continue;
                end
                
                rand_order = randperm(num_subj_arr(set_id));                
                dataArrForScatter{set_id} = dataArrForScatter{set_id}(rand_order(1:numberSubjectsToShow),:);                
            end
        end
        
        
        subplot(2,2,catch_id);                
        
        sTitle = CONST.catchLabelsArr{catch_id};
        
        bCreatePlot = 0; bSTD = 0; bSkipLegends = 0;
        utilsInternal_DotsPlot(dataArrForScatter,bCreatePlot,bSTD,...
                               [1,7],'items','average responses',sTitle,CONST.condNames, bSkipLegends);
                   
        ax=gca;
        ax.XAxis.FontSize = 16;
        ax.YAxis.FontSize = 16;
        ax.XLabel.FontSize = 16;
        ax.YLabel.FontSize = 16;
        
        b = 2;
    end
    
    if isnan(numberSubjectsToShow)    
        sTitle = ['All ratings, All subjects'];
    else
        sTitle = ['All ratings, numberSubjectsToShow ' num2str(numberSubjectsToShow)];
    end

    sgtitle(sTitle);
    b = 1;
    

function PlotBarGraphsPercentSubjects_byCatchCateg(CONST,DEMOGRArr, reproduceCorrArrArr,...
                                                   catchReportMatrixArr)                                               
                                               
    fig_ptr = figure('Position',[1 1 2000 1000]);
   
    num_catch_categ = length(CONST.catchLabelsArr);
    for catch_id=1:num_catch_categ

        num_groups = length(reproduceCorrArrArr);
        
        for file_id=1:num_groups      
            num_subjTotal = DEMOGRArr{file_id}.num_subj;
            num_subj_Passed = length(find(1 == catchReportMatrixArr{file_id}(:,catch_id)));
            perc_subjects(file_id) = (num_subj_Passed / num_subjTotal)*100;
        end
        
        subplot(2,2,catch_id);     

        h = bar(perc_subjects);
        set(h, 'FaceColor', 'y','BarWidth',0.4, 'EdgeColor','black',...
                'LineWidth',1,'BarWidth', 0.5); 
        %pbaspect([1 1 1]);    

        box off;
        
        ylabel('Percent subjects answered correctly');

        ax=gca;
        ax.YAxis.FontSize = 18;
        ax.YLabel.FontSize = 12;            
        title(CONST.catchLabelsArr{catch_id});
        set(gca,'XTickLabel',CONST.condNames);
        yticks([0 20 40 60 80 100]);
    end
        
    sgtitle('Percent correct resonses: by catch category');
    a = 1;

    
function [reproduceCorrArr,statsCacthCorrect,DEMOGR,...
          matrixData, catchReportMatrix,catchReportMatrix_BfrFailedCorrelFltr] = ...
            ProcessFile(inputFileName, CONST, file_id)

    tableData = readtable(inputFileName, 'PreserveVariableNames',true,'Delimiter', ',');
    
    matrixData_cell = table2cell(tableData);

    %Demographics processing
    DEMOGR.num_subj = size(matrixData_cell,1);
    
    genderDataVec = matrixData_cell(:,CONST.gender_colID);
    DEMOGR.num_males = sum(strcmp(genderDataVec, 'Male'));
    DEMOGR.num_females = sum(strcmp(genderDataVec, 'Female'));
    

    DEMOGR.ageDataVec = cell2mat(matrixData_cell(:,CONST.age_colID));
    DEMOGR.age_mean = mean(DEMOGR.ageDataVec,'omitnan');
    DEMOGR.age_std = std(DEMOGR.ageDataVec,'omitnan');

    handDataVec = matrixData_cell(:,CONST.hand_colID);
    DEMOGR.num_leftHand = sum(strcmp(handDataVec, 'left hand'));
    
    
    %first, we extract the data matrix (including catch trials)
    matrixData = cell2mat(matrixData_cell(:,CONST.dataColIds));    
    
   
    %filter subjects for whom we cannot calculate correlation (either completely identitical answers or almost)
    matrixData_BfrFailedCorrelFltr = matrixData;
    [matrixData,DEMOGR.numFailedCorrel,rows_excl_vec] = ...
        FilterSubjectsFailedCorrelationCalcNonCell(matrixData, CONST); 
    
    matrixData_cell_BfrFailedCorrelFltr = matrixData_cell;
    matrixData_cell(rows_excl_vec,:) = [];
    DEMOGR.ageDataVec_AfterFailedCorrelFltr = cell2mat(matrixData_cell(:,CONST.age_colID));
    
    
    %here we build matrix with binning per catch trial types
    catchReportMatrix = ProcessCatchTrials(matrixData,CONST.catchCorrectAns);
    catchReportMatrix_BfrFailedCorrelFltr = ProcessCatchTrials(matrixData_BfrFailedCorrelFltr,CONST.catchCorrectAns);    
    
    
    CalcFailedCorrelationPerCatches(CONST, catchReportMatrix_BfrFailedCorrelFltr,...
                                    matrixData_BfrFailedCorrelFltr);
    
    %the core calculation of reliability
    correlVec = CalcReliabilityBtwRepeats(matrixData,CONST);
       
    [num_subj,num_catch_conds] = size(catchReportMatrix);
    statsCacthCorrect = {'','num corr trials','percent corr trails','mean corr','std corr'};
    for catch_rule_id=1:num_catch_conds
        row_ids = find(1 == catchReportMatrix(:,catch_rule_id));
        statsCacthCorrect{catch_rule_id+1,1} = CONST.catchLabelsArr{catch_rule_id};
        statsCacthCorrect{catch_rule_id+1,2} = length(row_ids);
        statsCacthCorrect{catch_rule_id+1,3} = length(row_ids) / num_subj;

        correlVec_slice = correlVec(row_ids);
        statsCacthCorrect{catch_rule_id+1,4} = mean(correlVec_slice);
        statsCacthCorrect{catch_rule_id+1,5} = std(correlVec_slice);

        reproduceCorrArr{catch_rule_id} = correlVec_slice;
    end
    
    a = 1;

function CalcFailedCorrelationPerCatches(CONST, catchReportMatrix,matrixData) 

    qIDs_to_compare = CONST.qIDs_to_compare;
    [num_subj,num_catch_groups] = size(catchReportMatrix);
    failedCorrel_subj_codesVec_dbg = [];
    for catch_group_id=1:num_catch_groups
        failedCorrelCount = 0;
        totalCount = 0;
        summaryStats{1, catch_group_id+1} = CONST.catchLabelsArr{catch_group_id};
        
        if (1 == catch_group_id)
            summaryStats{2, 1} = 'total';
            summaryStats{3, 1} = 'failedCorrelCount';
            summaryStats{4, 1} = 'failedCorrelCountPerc';            
        end
        
        for subj_id=1:num_subj
            if (0 == catchReportMatrix(subj_id,catch_group_id))
                continue;
            end
            
            totalCount = totalCount + 1;
            
            vec1 = matrixData(subj_id,qIDs_to_compare{1});
            vec2 = matrixData(subj_id,qIDs_to_compare{2});

            [correl_R,correl_p_val] = corr([vec1',vec2'] ,'Type',CONST.correl_type);         
            if isnan(correl_R(1,2))
                failedCorrelCount = failedCorrelCount + 1;
                failedCorrel_subj_codesVec_dbg = [failedCorrel_subj_codesVec_dbg; subj_id];
            end
        end
        
        summaryStats{2,catch_group_id+1} = totalCount;
        summaryStats{3,catch_group_id+1} = failedCorrelCount;
        summaryStats{4,catch_group_id+1} = failedCorrelCount / totalCount;
    end
    
    a = 1;

function [matrixData,num_excl,rows_excl_vec] = FilterSubjectsFailedCorrelationCalcNonCell(matrixData, CONST)   
    
    qIDs_to_compare = CONST.qIDs_to_compare;
    rows_excl_vec = [];
    for subj_id=1:size(matrixData,1)
        vec1 = matrixData(subj_id,qIDs_to_compare{1});
        vec2 = matrixData(subj_id,qIDs_to_compare{2});

        [correl_R,correl_p_val] = corr([vec1',vec2'] ,'Type',CONST.correl_type);         
        if isnan(correl_R(1,2))
            rows_excl_vec = [rows_excl_vec; subj_id];
        end
    end
    
    num_excl = length(rows_excl_vec);
    matrixData(rows_excl_vec,:) = [];    

function correlVec = CalcReliabilityBtwRepeats(matrixData,CONST)

    subj_id_eff = 0;
    for subj_id=1:size(matrixData,1)
        vec1 = matrixData(subj_id,CONST.qIDs_to_compare{1});
        vec2 = matrixData(subj_id,CONST.qIDs_to_compare{2});

        [correl_R,correl_p_val] = corr([vec1',vec2'] ,'Type',CONST.correl_type);      
               
        if ~(isnan(correl_R(1,2)))
            subj_id_eff = subj_id_eff + 1;
            correlVec(subj_id,1) = correl_R(1,2);
        else
            error('We should not get NaN for correlation');
        end
    end
    a = 1;

% 
function catchReportMatrix = ProcessCatchTrials(matrixData,catchCorrectAns)

    for row_id=1:size(matrixData,1)
        subjLine = matrixData(row_id,:);
        
        for catch_set_id=1:length(catchCorrectAns)
            catchTblMap = catchCorrectAns{catch_set_id};
            
            [num_catch_q, last_map_columnID] = size(catchTblMap);
            bCorrectAnswArr = [];
            bCorrectAnswArr(1:num_catch_q) = 0;
            for catch_trial_id=1:num_catch_q
                data_col_id = catchTblMap(catch_trial_id,1);
                answer_given = subjLine(data_col_id);
                bCorrectAnswer = 0;
                for ans_col_id=2:last_map_columnID
                    answer_correct = catchTblMap(catch_trial_id,ans_col_id);
                    if (answer_given == answer_correct)
                        bCorrectAnswArr(catch_trial_id) = 1;
                        break;
                    end
                end
            end
            
            if isempty(find(bCorrectAnswArr == 0))
                catchReportMatrix(row_id,catch_set_id) = 1;
            else
                catchReportMatrix(row_id,catch_set_id) = 0;
            end
        end
    end
    a = 1;

    
function utilsInternal_DotsPlot(dataArr,bCreateFigure, bSTD, yAxisLimits,...
                                xLabelTxt, yLabelTxt, sTitle,...
                                legendTxt, bSkipLegends)

    color_codes = {'b','r','g','y'};

    if (bCreateFigure)
        fig_ptr = figure;   
        set(fig_ptr,'Position',[1 1 2000 1000]);
    end
    
    for series_id=1:length(dataArr)
        
        mean_tc = mean(dataArr{series_id});
        if (bSTD)
            error_tc = std(dataArr{series_id});
        else
            error_tc = utilsInternal_SEM_matrix(dataArr{series_id});
        end
        
        x = [1:length(mean_tc)];

        % Plot the data points using scatter
        hold on;

        % Plot the error bars
        %errorbar(x, mean_tc, error_tc, 'LineStyle', 'none', 'Color', color_codes{series_id});
        errorbar(x, mean_tc, error_tc,'o', 'MarkerFaceColor', color_codes{series_id},...
                'LineStyle', 'none', 'Color', color_codes{series_id});
        
    
        % Add labels and title
        xlabel(xLabelTxt);
        ylabel(yLabelTxt);
        title(sTitle);

        % Customize the plot
        grid off;
        hold off;
        box off;

        % Set custom X-axis limits to emphasize the gap
        ylim(yAxisLimits);
        xlim([0 length(x)+1]);

        % Set custom X-axis ticks to show only 1, 2, and 3
        xticks(x);
    end
    
    if (~bSkipLegends)
        legend(legendTxt,'Box', 'off','FontSize',10, 'location','northwest')
    end
    
    
function SEM = utilsInternal_SEM_matrix(data_mtrx)

    num_el = size(data_mtrx,1);
    SEM = nanstd(data_mtrx)/sqrt(num_el);   
    
    
function [fig_ptr,cohenD_out] = utilsInternal_IndivScatterPlotWithBars(cfg, Y_arr, column_labels,...
                                                                       YAxisName, titleName, bBarsOnly)
                                                       
    cohenD_out = NaN;
    fig_ptr = NaN;
                                                
    if ~isfield(cfg,'bCreateFigure')
       cfg.bCreateFigure = 1;
    end

    if ~isfield(cfg,'bPairedTTest')
       cfg.bPairedTTest = 1;
    end
    
    if ~isfield(cfg,'bPlotZeroLine')
       cfg.bPlotZeroLine = 0;
    end    
    
    if ~isfield(cfg,'bParallelcoords')
       cfg.bParallelcoords = 0;
    end      
    
    if ~isfield(cfg,'bMeanExclNaN')
       cfg.bMeanExclNaN = 0;
    end  
    
    if ~isfield(cfg,'bStatsShowMode')
       cfg.bStatsShowMode = 1;
    end      
    
    if ~isfield(cfg,'FontSizeStruct')
       cfg.bFontsSpecified = 0;
    else
        cfg.bFontsSpecified = 1;
    end    
    
    if ~isfield(cfg,'IndividualPointFactor')
       cfg.IndividualPointFactor = 10;
    end          

    if ~isfield(cfg,'AveragePointFactor')
       cfg.AveragePointFactor = 5;
    end      
    
    if ~isfield(cfg,'BarFaceColor')
       cfg.BarFaceColor = 'w';
    else
        cfg.BarFaceColor = [0.8 0.8 0.8];
    end      
    
    
    if (cfg.bCreateFigure)
        fig_ptr = figure;      
        set(fig_ptr,'Position',[1 1 2000 1000]);
        set(fig_ptr, 'PaperPositionMode', 'auto');
    end                                               

    % Number of variables
    num_col = length(Y_arr); 
    siz = 1;
    x = [];
    col = [0 0 1];

    % Standard x range?
    if isempty(x)
        x = 1:num_col;
    end

    % All same colour?
    if size(col,1) == 1
        col = repmat(col, num_col, 1);
    end
    
    
    
    bAllColumnTheSameSize = 1;
    for col_id=2:length(Y_arr)
        if (length(Y_arr{col_id}) ~= length(Y_arr{col_id-1}))
            bAllColumnTheSameSize = 0;
            break;
        end
    end
    
    
    %title and statistics for title
    titleStr{1} = titleName;
    
    if (1 == cfg.bStatsShowMode)
    
        if (3 == length(Y_arr)) && bAllColumnTheSameSize
            test_pairs = nchoosek(1:3,2);
            for pair_id=1:size(test_pairs,1)
                if (cfg.bPairedTTest)
                    [h,p,ci,stats] = ttest(Y_arr{test_pairs(pair_id,1)},Y_arr{test_pairs(pair_id,2)}, 'Tail','both'); 
                else
                    [h,p,ci,stats] = ttest2(Y_arr{test_pairs(pair_id,1)},Y_arr{test_pairs(pair_id,2)}, 'Tail','both'); 
                end

                titleStr{pair_id + 1} = sprintf('%s-%s tstat:%.2f, pval:%.3f, df:%d',...
                                        column_labels{test_pairs(pair_id,1)},...
                                        column_labels{test_pairs(pair_id,2)},...
                                        stats.tstat,p, stats.df);
            end

        elseif (2 == length(Y_arr)) || ~bAllColumnTheSameSize
            if (cfg.bPairedTTest)
                
                if (length(Y_arr{1}) < 2)
                    p = NaN;  stats.tstat = NaN; percEff = NaN;
                else                
                    [h,p,ci,stats] = ttest(Y_arr{1},Y_arr{2}, 'Tail','both'); 
                    mean1 = mean(Y_arr{1});
                    mean2 = mean(Y_arr{2});
                    if (mean1 > mean2)
                        vecDiff = Y_arr{1} - Y_arr{2};
                    else
                        vecDiff = Y_arr{2} - Y_arr{1};
                    end
                    percEff = length(find(vecDiff > 0)) / length(vecDiff);
                                        
                    cohenD = (mean(Y_arr{1}) - mean(Y_arr{2}))/stats.sd;
                end
                
                 titleStr{2} = sprintf('%s-%s tstat:%.2f, pval:%.3f df:%d cohenD:%.2f perc.Eff:%.2f',...
                                       column_labels{1},column_labels{2},...
                                       stats.tstat, p, stats.df, cohenD, percEff);      
                                  
                if (p<0.05)
                    titleStr{2} = ['\color{red}' titleStr{2}];
                end                                  
            else
                [h,p,ci,stats] = ttest2(Y_arr{1},Y_arr{2}, 'Tail','both'); 
                sd_pooled = sqrt((std(Y_arr{1},'omitnan')^2 + std(Y_arr{2},'omitnan')^2)/2);
                cohenD = (mean(Y_arr{1},'omitnan') - mean(Y_arr{2},'omitnan'))/sd_pooled;
                cohenD_out = cohenD;
                
                titleStr{2} = sprintf('%s-%s tst:%.2f, p:%.3f, df:%d cohD: %.2f CI:[%.2f:%.2f]',...
                                      column_labels{1},column_labels{2},...
                                      stats.tstat, p, stats.df, cohenD,ci(1),ci(2)); 
                if (p<0.05)
                    titleStr{2} = ['\color{red} ' titleStr{2}];
                end
            end
        else
            titleStr{2}  = '';
        end
    end
    
    for col_id=1:length(Y_arr)
        
        if isempty(Y_arr{col_id}) || length(Y_arr{col_id}) < 2
            oneSample_pValArr(col_id) = NaN;
            oneSample_tStatArr(col_id) = NaN;
            oneSample_cohenD(col_id) = NaN;
        else            
            [h,p,ci,stats] = ttest(Y_arr{col_id});            
            oneSample_pValArr(col_id) = p;
            oneSample_tStatArr(col_id) = stats.tstat;        
            oneSample_cohenD(col_id) = stats.tstat/sqrt(length(Y_arr{col_id}));
        end
    end    
    
    for col_id = 1:num_col
        if (cfg.bMeanExclNaN)
            mean_arr(col_id) = mean(Y_arr{col_id},'omitnan');
        else
            mean_arr(col_id) = mean(Y_arr{col_id});
        end
        SEM_arr(col_id) = utilsInternal_SEM_matrix(Y_arr{col_id});
    end
    
    
    h = bar(mean_arr);
    set(h, 'FaceColor', cfg.BarFaceColor,'BarWidth',0.4, 'EdgeColor',...
            'black','LineWidth',2,'BarWidth', 0.5); 
    hold on;
    errorbar(1:length(SEM_arr),mean_arr,SEM_arr,'black','LineStyle', 'none','LineWidth', 2);
    
    % Plot cat eyes
    hold on
    h = []; 
    for col_id = 1:num_col
        if (~bBarsOnly)        
            if (~cfg.bParallelcoords)
                scatter(randn(length(Y_arr{col_id}),1)/15 + x(col_id), Y_arr{col_id}, cfg.IndividualPointFactor*siz, ...
                        'o', 'markeredgecolor', col(col_id,:), 'markerfacecolor', col(col_id,:)); % Plot individual subjects                    
            end

            if (cfg.bMeanExclNaN)
                meanVal = mean(Y_arr{col_id},'omitnan');
            else
                meanVal = mean(Y_arr{col_id});
            end

            SEMVal = utilsInternal_SEM_matrix(Y_arr{col_id});
            
            num_dataPoints = length(Y_arr{col_id});
        end
        
        
        if (1 == cfg.bStatsShowMode)
            
            if (num_col > 5)
                sFormatterStr = '%s\\newline:%.2f(%.2f)\\newlineNPo:%d\\newlinep:%.3f, t:%.1f\\newlineCohenD:%.2f';
            else
                sFormatterStr = '%s:%.2f(%.2f)\\newlineNPo:%d\\newlinep:%.3f, t:%.1f\\newlineCoD:%.2f';
            end
                
            column_labelsWithMean{col_id} = sprintf(sFormatterStr,...
                                               column_labels{col_id},meanVal,SEMVal,...
                                               num_dataPoints,...
                                               oneSample_pValArr(col_id),...
                                               oneSample_tStatArr(col_id),...
                                               oneSample_cohenD(col_id));
        else
            column_labelsWithMean{col_id} = column_labels{col_id};
        end
    end
    
    xticks([1:num_col]);
    set(gca,'XTickLabel',column_labelsWithMean,'FontSize',5);    

    title([titleStr],'FontSize',6);
    ylabel(YAxisName,'Interpreter', 'none');
    
    if (cfg.bPlotZeroLine)
        plot([0,num_col], [0 0], 'Color',[0.5,0.5,0.5],  'LineStyle','-');          
    end
    
    if (cfg.bParallelcoords)
        measures = [];
        for cond_id=1:length(Y_arr)
            measures = [measures Y_arr{cond_id}];
        end
        parallelcoords(measures, 'Color', [0 0 1], ...
                       'LineStyle', '-', 'Marker', '.', 'MarkerSize', cfg.IndividualPointFactor);    
    end
    
    hold off
    

    set(gca, 'xtick', x(1):x(end));
    xlim([0 x(end)+1]);
    
    
    Y_all = [];
    for cond_id=1:length(Y_arr)
        Y_all = [Y_all; Y_arr{cond_id}];        
    end
    minVal = min(Y_all);
    maxVal = max(Y_all);
    range = maxVal - minVal;
    if (range ~= 0)
        ylim([minVal-range*0.05 maxVal+range*0.05]);
    end
        
    if (cfg.bFontsSpecified)
        ax=gca;
        ax.XAxis.FontSize = cfg.FontSizeStruct.XAxis.FontSize;
        ax.YAxis.FontSize = cfg.FontSizeStruct.YAxis.FontSize;
        ax.XLabel.FontSize = cfg.FontSizeStruct.XLabel.FontSize;
        ax.YLabel.FontSize = cfg.FontSizeStruct.YLabel.FontSize;
        ax.Title.FontSize = cfg.FontSizeStruct.Title.FontSize;
    end
    
    box off;


    