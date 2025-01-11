% =========================================================================
% This script performs robustness checks by ploting the RMSE of alternative
% model specifications compared to the benchmark model. 
% =========================================================================

% Models:
mod_names = [shrk_names tree_names];                                        % Combine short model names
nMod      =  nshrk + ntree;                                                 % Total number of competing models

% Load data on IPCA and SPF nowcasts (Year-on-Year % change):
cd(dir_database)
ipcafname  = 'IPCA_SPF_YoY.xlsx';                                           % File name
oosDate    = table2array(readtable(ipcafname,'Range','A:A'));               % Monthly dates of the evaluation period
T          = size(oosDate,1);                                               % Number of monthly out-of-sample periods
IPCA       = table2array(readtable(ipcafname,'Range','B:B'));               % Official IPCA rates
SPFname    = {'SPF median','SPF Top5'};                                     % SPF benchmark
SPF(:,:,1) = table2array(readtable(ipcafname,'Range','C:F'));               % SPF nowcasts: Median
SPF(:,:,2) = table2array(readtable(ipcafname,'Range','G:J'));               % SPF nowcasts: Top 5
for m = 1:2
    D0(:,:,m)  = IPCA - SPF(:,:,m);                                         % Nowcast error of the SPF benchmark
    RMSE0(m,:) = sqrt(mean(D0(:,:,m).^2));                                  % RMSE values of the SPF benchmark
end

% Load data on model nowcasts (Year-on-Year % change):
cd(dir_results);                                               % Set directory to baseline model results
D_base    = NaN(T,ndays,nMod);                                 % Pre-allocate matrix of baseline nowcast errors
RMSE_base = nan(nMod,ndays);                                   % Pre-allocate matrix of RMSE 
for m = 1:nMod                                                 % Loop over models
    y_hat_m  = rmmissing(table2array(readtable(append('results_',models{m},'.xlsx'),'Range','B:E'))); % Load baseline nowcasts
    D_base(:,:,m) = IPCA - y_hat_m;                            % Baseline nowcast errors
    RMSE_base(m,:) = sqrt(mean(D_base(:,:,m).^2));             % RMSE values of the baseline strategy
end
compt_model_spec = {'a_high_frequency_lags','b_ragged_edge','c_SPF_predictor'}; % Set competing model specifications
base_names  = {'$p=0$','no ragged edges','with SPF'};          % Define names (distinguishing characteristic) for the baseline specification
compt_names = {'$p=3$','with ragged-edge problem','without SPF'};% Define names (distinguishing characteristic) for the competing specifications
ncompt      = length(compt_model_spec);                        % Number of competing specifications
D_compt     = NaN(T,ndays,nMod,ncompt);                        % Pre-allocate matrix of benchmark nowcast errors
RMSE_compt  = nan(nMod,ndays,ncompt);                          % Pre-allocate matrix of RMSE 
for si = 1:ncompt                                              % Loop over competing specifications
    for m = 1:nMod                                             % Loop over models
        cd(append(dir_robust,'/',compt_model_spec{si})); % Set directory to corresponding strategy
        y_hat_m  = rmmissing(table2array(readtable(append('results_',models{m},'.xlsx'),'Range','B:E'))); % Load benchmark nowcasts
        D_compt(:,:,m,si) = IPCA - y_hat_m;                    % Benchmark nowcast errors
        RMSE_compt(m,:,si) = sqrt(mean(D_compt(:,:,m,si).^2)); % RMSE values
    end
end

% Plot 1: high-frequency lags
si = 1;
cd(dir_figures)
cl = {'#0072BD','#A2142F','#77AC30','#EDB120'};
topmod = [1 2 5 6 7 8]; botmod = [3];
figure
clear plt lgtext
tl = tiledlayout(1,1,'TileSpacing','tight','Padding','tight');
nexttile, hold on
for i = 1:ndays
    plt(i) = scatter(RMSE_compt([topmod botmod],i,si),RMSE_base([topmod botmod],i),180,'filled','MarkerFaceColor',cl{i},'MarkerEdgeColor','none','MarkerFaceAlpha',0.8);
    lgtext{i} = ['Day ' int2str(mdays(i))]; if mdays(i) == 99, lgtext{i} = 'End-of-month'; end
    %text(RMSE_compt(:,i,si),RMSE_base(:,i)+0.01,mod_names,'Color',0.25*[1 1 1],'FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
    text(RMSE_compt(topmod,i,si),RMSE_base(topmod,i)+0.01,mod_names(topmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
    text(RMSE_compt(botmod,i,si),RMSE_base(botmod,i)-0.01,mod_names(botmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
end
grid on
xlim([0.097 0.31])
ylim([0.097 0.30])
text(0.1,0.2,'$\;\;$ Outperformance $\uparrow$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','left','VerticalAlignment','bottom')
text(0.31,0.15,'Underperformance $\downarrow \;\;$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','bottom')
text(0.295,0.27,'$-20\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','left','VerticalAlignment','top')
hline = refline([1 0]); hline.Color = 'k'; hline.LineStyle = '-'; hline.LineWidth = 2;
plot(linspace(0.12,0.36),linspace(0.1,0.3),'Color',0.1*[1 1 1],'LineStyle','--','LineWidth',1)
set(gca,'FontSize',18)
set(gca,'TickLabelInterpreter','latex','FontSize',18)
xlabel(append('Alternative RMSE (with ',compt_names{si},')'),'FontSize',20,'Interpreter','latex')
ylabel(append('Baseline RMSE (with ',base_names{si},')'),'FontSize',20,'Interpreter','latex')
legend(plt,lgtext,'Location','northwest','Orientation','Horizontal','Interpreter','Latex','FontSize',20);
set(gcf,'Position',[100 100 1000 400])
exportgraphics(tl,[pwd sprintf('/Robustness_%s.png',compt_model_spec{si})],'Resolution',500)
close all

% Plot 3: SPF median as predictor
si = 3;
cl = {'#0072BD','#A2142F','#77AC30','#EDB120'};
topmod = [1 2 5 6 7]; botmod = [3 4 8];
figure
clear plt lgtext
tl = tiledlayout(1,1,'TileSpacing','tight','Padding','tight');
nexttile, hold on
for i = 1:ndays
    plt(i) = scatter(RMSE_compt([topmod botmod],i,si),RMSE_base([topmod botmod],i),180,'filled','MarkerFaceColor',cl{i},'MarkerEdgeColor','none','MarkerFaceAlpha',0.8);
    lgtext{i} = ['Day ' int2str(mdays(i))]; if mdays(i) == 99, lgtext{i} = 'End-of-month'; end
    text(RMSE_compt(topmod,i,si),RMSE_base(topmod,i)+0.01,mod_names(topmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
    text(RMSE_compt(botmod,i,si),RMSE_base(botmod,i)-0.01,mod_names(botmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
end
grid on
xlim([0.14 0.32])
ylim([0.1 0.31])
text(0.14,0.2,'$\;\;$ Outperformance $\uparrow$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','left','VerticalAlignment','bottom')
text(0.32,0.15,'Underperformance $\downarrow \;\;$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','bottom')
text(0.32,0.26,'$-20\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
text(0.32,0.222,'$-40\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
hline = refline([1 0]); hline.Color = 'k'; hline.LineStyle = '-'; hline.LineWidth = 2;
plot(linspace(0.12,0.31),linspace(0.1,0.2583),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
plot(linspace(0.14,0.31),linspace(0.1,0.2214),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
set(gca,'FontSize',18)
set(gca,'TickLabelInterpreter','latex','FontSize',18)
xlabel(append('Alternative RMSE (',compt_names{si},')'),'FontSize',20,'Interpreter','latex')
ylabel(append('Baseline RMSE (',base_names{si},')'),'FontSize',20,'Interpreter','latex')
legend(plt,lgtext,'Location','northwest','Orientation','Horizontal','Interpreter','Latex','FontSize',20);
set(gcf,'Position',[100 100 1000 400])
exportgraphics(tl,[pwd sprintf('/Robustness_%s.png',compt_model_spec{si})],'Resolution',500)
close all

% Plot 2: ragged-edge problem
si = 2;
cl = {'#0072BD','#A2142F','#77AC30','#EDB120'};
topmod = [1 2 4 5 7 8]; botmod = [3 6];
figure
clear plt lgtext
tl = tiledlayout(1,1,'TileSpacing','tight','Padding','tight');
nexttile, hold on
for i = 1:ndays
    plt(i) = scatter(RMSE_compt([topmod botmod],i,si),RMSE_base([topmod botmod],i),180,'filled','MarkerFaceColor',cl{i},'MarkerEdgeColor','none','MarkerFaceAlpha',0.8);
    lgtext{i} = ['Day ' int2str(mdays(i))]; if mdays(i) == 99, lgtext{i} = 'End-of-month'; end
    text(RMSE_compt(topmod,i,si),RMSE_base(topmod,i)+0.01,mod_names(topmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
    text(RMSE_compt(botmod,i,si),RMSE_base(botmod,i)-0.01,mod_names(botmod),'Color','k','FontSize',14,'Interpreter','latex','HorizontalAlignment','center');
end
grid on
xlim([0.097 0.27])
ylim([0.097 0.30])
text(0.1,0.2,'$\;\;$ Outperformance $\uparrow$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','left','VerticalAlignment','bottom')
text(0.27,0.15,'Underperformance $\downarrow \;\;$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','bottom')
text(0.27,0.23,'$-20\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
text(0.265,0.29,'$+20\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
%text(0.32,0.222,'$-40\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
%text(0.32,0.194,'$-60\%$','Color','k','Interpreter','latex','FontSize',20,'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','middle')
hline = refline([1 0]); hline.Color = 'k'; hline.LineStyle = '-'; hline.LineWidth = 2;
plot(linspace(0.12,0.31),linspace(0.1,0.2583),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
plot(linspace(0.1,0.2583),linspace(0.12,0.31),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
%plot(linspace(0.14,0.31),linspace(0.1,0.2214),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
%plot(linspace(0.16,0.31),linspace(0.1,0.1938),'Color',0.25*[1 1 1],'LineStyle','--','LineWidth',1)
set(gca,'FontSize',18)
set(gca,'TickLabelInterpreter','latex','FontSize',18)
xlabel(append('Alternative RMSE (',compt_names{si},')'),'FontSize',20,'Interpreter','latex')
ylabel(append('Baseline RMSE (',base_names{si},')'),'FontSize',20,'Interpreter','latex')
legend(plt,lgtext,'Location','northwest','Orientation','Horizontal','Interpreter','Latex','FontSize',20);
set(gcf,'Position',[100 100 1000 400])
exportgraphics(tl,[pwd sprintf('/Robustness_%s.png',compt_model_spec{si})],'Resolution',500)
close all