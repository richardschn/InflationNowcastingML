% =========================================================================
% This script automatically generates the figures in "Harnessing Machine
% Learning for Real-Time Inflation Nowcasting".
% R. Schnorrenberger, A. Schmidt and G. V. Moura (2024).
% =========================================================================

% Clear working environment and set baseline directories:
clear, clc       
addpath('data/','figures/','functions/')
dir_main     = cd;
dir_figures  = append(dir_main,'/figures');
dir_data     = append(dir_main,'/data');
dir_database = append(dir_data,'/database');
dir_results  = append(dir_data,'/results');
dir_coef     = append(dir_data,'/coef');
dir_robust   = append(dir_results,'/robustness');

% Define common models and baseline specs:
models     = {'LASSO','Ridge','ElasticNet','sgLASSO','RF','LLF','BART','LASSOLLF'}; % Name exactly like in "Main.R"
shrk_names = {'LASSO', 'Ridge', 'Elastic Net', 'sg-LASSO'};                 % Short name as in the paper: shrinkage-based models
tree_names = {'RF', 'LLF', 'BART', 'LASSO-LLF'};                            % Short name as in the paper: tree-based models
nshrk      = length(shrk_names);                                            % Number of shrinkage-based models
ntree      = length(tree_names);                                            % Number of tree-based models
mdays      = [8, 15, 22, 99];                                               % Relevant days of the nowcast
ndays      = length(mdays);                                                 % Number of nowcasting days

% Define functions to execute:
fig_names = {'CUMSFE.m','Robustness.m','LASSO_coef.m'};

% Store current workspace:
globalVars = who; globalVars{end+1} = 'globalVars';

% Loop over figures:
for i = 1:length(fig_names)
    run(fig_names{i});                                                      % Generate figure
    cd(dir_main)                                                            % Reset directory to baseline folder
    clearvars('-except',globalVars{:})                                      % Clear all variables except global ones
end