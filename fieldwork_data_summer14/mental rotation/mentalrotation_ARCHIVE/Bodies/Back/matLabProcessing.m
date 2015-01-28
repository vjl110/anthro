%create subject list to iterate through
sublist = 1 %[1:10,12];

%create empty matrix to assign data to
data = [ ];

dataline2 = 1;

for subnum = sublist(1:end)
    filename = strcat('Subject', num2str(subnum), '.txt')
    fid = fopen(filename);
    
    
    line = fgetl(fid);
    line = fgetl(fid);
    line = fgetl(fid);
    %age = str2num(line(regexp(line, '[\d]')));
    
    line=fgetl(fid);
    input_female = 'f(emale)?';
    input_male = 'm(ale)?';
    if regexpi(line, input_female);
        sex = 0;
    elseif regexpi(line, input_male);
        sex = 1;
    else
        sex = 999;
    end
    
    fgetl(fid);
    fgetl(fid);
    fgetl(fid);
    fgetl(fid);
    fgetl(fid);
    % now you'll be through the header information and at the first line of data
    
    
    while cellfun(@isempty,dataline2) == 0
        
        dataline = textscan(fid, '%f %s %s %f %s %f', 1);
        dataline2 = dataline;
        if cellfun(@isempty,dataline2) == 0
            ttrialnum = dataline{1};
            if regexp(cell2mat(dataline{2}), 'Male_Back')==1
                exptype = 0;
            elseif regexp(cell2mat(dataline{2}), 'Male_Front')==1
                exptype = 1;
            end
            if regexp(cell2mat(dataline{3}), 'Left')==1
                corrima = 0;
            elseif regexp(cell2mat(dataline{3}), 'Right')==1
                corrima = 1;
            end
            deg = dataline{4};
            if regexp(cell2mat(dataline{5}), 'Left')==1
                resp = 0;
            elseif regexp(cell2mat(dataline{5}), 'Right')==1
                resp = 1;
            end
            rt = dataline{6};
            tempdata = cat(2, ttrialnum, exptype, corrima, deg, resp, rt)
            if cellfun(@isempty,dataline2) == 0
                data = cat(1, data, tempdata);
                 
            end
        end
    end
end
% This should read through all of the data and save it in a matrix called 'data'