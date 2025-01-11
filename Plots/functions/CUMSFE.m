% =========================================================================
% This script evaluates the forecasting performance of the competing models
% in relation to the SPF benchmark (median or Top 5) by means of cumulative
% sum of squared forecast errors (CUMSFE) throughout the evaluation period
% for each nowcasting day.
% =========================================================================

% Load data on nowcasting errors: data matrix = [observed data, mdays nowcasts, mdays Focus expectations]
cd(dir_results)
for m = 1:nshrk+ntree
    fname = append('results_',models{m});
    data  = rmmissing(table2array(readtable(fname,'Sheet','Results','VariableNamingRule','preserve','Range','A:I')));
    D(:,:,m) = data(:,2:5)-data(:,1);
end
D_bench = data(:,6:end)-data(:,1);

% Prepare dates:
T = size(D,1);                                                             
for i = 1:ndays
    if mdays(i)==99
        ndayi = datetime(2013,01,31)+calmonths(0:T-1)';
        n_lg{i} = 'End-of-month';
    else 
        ndayi = datetime(2013,01,mdays(i))+calmonths(0:T-1)';
        n_lg{i} = ['Day ' int2str(mdays(i))];
    end
    if i==1, ndates = ndayi; else ndates = [ndates, ndayi]; end
end
inflation_periods = datenum([datetime(2015,01,01) datetime(2016,02,28); datetime(2020,09,01) datetime(2022,06,30)]);

% Plot: Shrinkage
cd(dir_figures)
figure
tl = tiledlayout(nshrk,1,'TileSpacing','compact','Padding','compact');
cl = {'#0072BD','#A2142F','#77AC30','#EDB120'};
for m = 1:nshrk
    ax = nexttile;
    for i = 1:ndays
        p(i) = stairs(ndates(:,i),cumsum(D_bench(:,i).^2 - D(:,i,m).^2),'Color',cl{i},'LineWidth',3);
        hold on
    end
    grid on
    yline(0,'LineWidth',1.5)
    ylim([-1 2.5])
    text(ndates(1),+0.5,'$\;\;$ Outperformance $\uparrow$','Color',0.1*[1 1 1],'Interpreter','latex','FontSize',20)
    text(ndates(1),-0.5,'Underperformance $\downarrow$','Color',0.1*[1 1 1],'Interpreter','latex','FontSize',20)
    yyaxis right
    p(i+1) = bar(dateshift(ndates(:,end),'start','month'),data(:,1),'FaceAlpha',0.25,'FaceColor',0.4*[1 1 1],'EdgeAlpha',0);
    ylim([2 14])
    hold off
    xline([datetime(2014,07,01) datetime(2020,04,01)],'--',{'Domestic Economic Crisis','COVID-19 Crisis'},...
        'Color',0.35*[1 1 1],'LineWidth',1,'LabelOrientation','horizontal','LabelHorizontalAlignment','right','LabelVerticalAlignment','top','Interpreter','latex','FontSize',18)
    % recessionplot('recessions',inflation_periods)
    ax = gca;
    ax.YAxis(1).Color = 'k';
    ax.YAxis(2).Color = 'k';
    box off
    set(gca,'FontSize',18)
    set(gca,'TickLabelInterpreter','latex','FontSize',18)
    title(shrk_names{m},'Interpreter','latex','FontSize',22,'FontWeight','bold')
end
lg = legend(ax,p,[n_lg 'Inflation Rate (YoY \% change, right axis)'],'Orientation','horizontal','Box','off','interpreter','Latex','FontSize',22);
lg.Layout.Tile = 'South';
set(gcf,'Position',[100 100 1000 1300])
exportgraphics(tl,[pwd append('/CUMSFE_shrinkage.png')],'Resolution',500);
close all

% Plot: Tree
figure
tl = tiledlayout(ntree,1,'TileSpacing','compact','Padding','compact');
cl = {'#0072BD','#A2142F','#77AC30','#EDB120'};
for m = 1:ntree
    ax = nexttile;
    for i = 1:ndays
        p(i) = stairs(ndates(:,i),cumsum(D_bench(:,i).^2 - D(:,i,nshrk+m).^2),'Color',cl{i},'LineWidth',3);
        hold on
    end
    grid on
    yline(0,'LineWidth',1.5)
    ylim([-3.2 1.5])
    text(ndates(1),+0.5,'$\;\;$ Outperformance $\uparrow$','Color',0.1*[1 1 1],'Interpreter','latex','FontSize',20)
    text(ndates(1),-1,'Underperformance $\downarrow$','Color',0.1*[1 1 1],'Interpreter','latex','FontSize',20)
    yyaxis right
    p(i+1) = bar(dateshift(ndates(:,end),'start','month'),data(:,1),'FaceAlpha',0.25,'FaceColor',0.4*[1 1 1],'EdgeAlpha',0);
    ylim([2 14])
    hold off
    xline([datetime(2014,07,01) datetime(2020,04,01)],'--',{'Domestic Economic Crisis','COVID-19 Crisis'},...
        'Color',0.35*[1 1 1],'LineWidth',1,'LabelOrientation','horizontal','LabelHorizontalAlignment','right','LabelVerticalAlignment','top','Interpreter','latex','FontSize',18)
    % recessionplot('recessions',inflation_periods)
    ax = gca;
    ax.YAxis(1).Color = 'k';
    ax.YAxis(2).Color = 'k';
    box off
    set(gca,'FontSize',18)
    set(gca,'TickLabelInterpreter','latex','FontSize',18)
    title(tree_names{m},'Interpreter','latex','FontSize',22,'FontWeight','bold')
end
lg = legend(ax,p,[n_lg 'Inflation Rate (YoY \% change, right axis)'],'Orientation','horizontal','Box','off','interpreter','Latex','FontSize',22);
lg.Layout.Tile = 'South';
set(gcf,'Position',[100 100 1000 1300])
exportgraphics(tl,[pwd append('/CUMSFE_tree.png')],'Resolution',500);
close all