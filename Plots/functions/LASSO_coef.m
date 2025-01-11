% =========================================================================
% This script generates a heatmap of LASSO coefficient estimates for each 
% nowcasting day.
% =========================================================================

% Load data and set parameters:
cd(dir_coef)
model       = 'LASSO';
fixed_coef  = {'Const','Y1','IPCA15','IGPM','IGP10','IPAM','IPADI','INCCM'};
vary_coef   = {'IPCS','FIPE','DIESEL','GAS','ETOH','LNG','SELIC','FOREX','IBOV','IEE','DI10','SPREAD','BCOM','EXPt0'};
seasdumm    = {'M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11'};
for i = 1:ndays
    data = readtable('coef_LASSO.xlsx','VariableNamingRule','preserve','Sheet',append('Day_',int2str(mdays(i))));
    Varnames = [fixed_coef append(vary_coef,int2str(i)) seasdumm];
    coef_data(:,:,i) = table2array(data(:,Varnames))';
end
N  = size(coef_data,1);
T  = size(coef_data,2);
ylabels = [fixed_coef vary_coef seasdumm];
xlabels = cellstr(datestr(datetime(2013,01,01)+calmonths(0:T-1),'yyyy'))';

CustomXLabels = string(xlabels(2:end));
CustomXLabels(mod(1:T,12) ~= 0) = " ";
CustomXLabels = [xlabels(1) CustomXLabels(1:end-1)];

% Generate heatmaps:
cd(dir_figures)
for i = 1:ndays
    Mplot = coef_data(:,:,i);
    Mplot(Mplot==0) = NaN;
    figure
    tiledlayout(1,1,'TileSpacing','tight','Padding','compact');
    nexttile; 
    h = heatmap(1:T,ylabels,Mplot,'Colormap',copper,'GridVisible','off','CellLabelColor','None','FontName','Times New Roman','FontSize',16,'MissingDataColor',[1 1 1]);
    h.NodeChildren(3).Title.Interpreter = 'latex';
    h.XDisplayLabels = CustomXLabels;
    %h.ColorLimits = [-0.2 0.2];
    s = struct(h);
    s.XAxis.TickLabelRotation = 0;
    set(gcf,'Position',[100 100 600 500])
    exportgraphics(h,[pwd append('/coef_',model,'_nday',num2str(i),'.png')],'Resolution',300)
    close all
end


% =========================================================================
% This script generates a bar plot of variable importance via the LASSO 
% grouped into different categories of predictors on each nowcasting day.
% =========================================================================

% Generate bar plot:
group_idx = [find(string(ylabels)=='IPCA15') find(string(ylabels)=='INCCM'); find(string(ylabels)=='IPCS') find(string(ylabels)=='LNG');...
            find(string(ylabels)=='SELIC') find(string(ylabels)=='BCOM'); find(string(ylabels)=='EXPt0') find(string(ylabels)=='EXPt0')];
groups    = {'Monthly price indicators','Weekly price indicators','Daily financial variables','Survey-based expectations'};
mdates    = datetime(2013,01,01)+calmonths(0:T-1)';
splits    = {'Domestic Economic Crisis','Pre-Pandemic','Post-Pandemic'};
dt_splits = [1 find(mdates=='01-Dec-2016'); find(mdates=='01-Dec-2016')+1 find(mdates=='01-Mar-2020'); find(mdates=='01-Mar-2020')+1 length(mdates)];

y = NaN(ndays,size(group_idx,1),size(dt_splits,1));
for k = 1:size(dt_splits,1)
    for g = 1:size(group_idx,1)
        for i = 1:ndays
            Bplot = coef_data(group_idx(g,1):group_idx(g,2),dt_splits(k,1):dt_splits(k,2),i);
            Bplot(isnan(Bplot)) = 0;
            y(i,g,k) = sum(abs(Bplot),"all");    
        end
    end
    y(:,:,k) = y(:,:,k)./repmat(sum(y(:,:,k),2),[1 size(group_idx,1)]);
end

X = categorical({'Day 8','Day 15','Day 22','EoM'});
X = reordercats(X,{'Day 8','Day 15','Day 22','EoM'});
cl = {'#054FB9','#C44601','#5BA300','#9B8BF4'}; 

cd(dir_figures)
figure
tl = tiledlayout(1,size(dt_splits,1),'TileSpacing','tight','Padding','tight');
clear plt 
for k = 1:size(dt_splits,1)
    px = nexttile; hold on
    plt = bar(X,y(:,:,k),'stacked');
    colororder(cl)
    if k>1, set(gca,'YColor','none'); end
    grid on
    set(gca,'FontSize',18)
    set(gca,'TickLabelInterpreter','latex','FontSize',18)
    title(splits(k),'Interpreter','latex','FontSize',20,'FontWeight','bold') 
end
lg = legend(px,plt,groups,'Orientation','Horizontal','Box','off','interpreter','Latex','FontSize',20);
lg.Layout.Tile = 'South';
set(gcf,'Position',[10 10 1050 300])
exportgraphics(tl,[pwd append('/group_relevance_',model,'.png')],'Resolution',600)
close all