classdef CapiProxy < handle
    properties (Access = private)
        Process
        ProxyUrl
    end

    methods
        function obj = CapiProxy()
            obj.Process = [];
            obj.ProxyUrl = '';
        end

        function url = start(obj)
            % Spawn: npx tsx ../../../test/harness/server.ts
            % from the e2e directory, the harness is at ../../../test/harness/server.ts
            % but we run npx with cwd set to the harness directory

            harnessDir = fullfile(fileparts(mfilename('fullpath')), '..', '..', '..', 'test', 'harness');
            harnessDir = char(java.io.File(harnessDir).getCanonicalPath());
            serverPath = fullfile(harnessDir, 'server.ts');

            % Use Java ProcessBuilder for better control
            pb = java.lang.ProcessBuilder(java.util.Arrays.asList({'npx', 'tsx', serverPath}));
            pb.directory(java.io.File(harnessDir));
            pb.redirectErrorStream(false);

            obj.Process = pb.start();

            % Read first line from stdout
            reader = java.io.BufferedReader(java.io.InputStreamReader(obj.Process.getInputStream()));
            line = char(reader.readLine());

            % Parse "Listening: http://..."
            tokens = regexp(line, 'Listening: (http://[^\s]+)', 'tokens');
            if isempty(tokens)
                obj.Process.destroy();
                error('Failed to parse proxy URL from: %s', line);
            end

            obj.ProxyUrl = tokens{1}{1};
            setenv('COPILOT_API_URL', obj.ProxyUrl);
            url = obj.ProxyUrl;
        end

        function stop(obj)
            if ~isempty(obj.ProxyUrl)
                try
                    webwrite([obj.ProxyUrl '/stop'], '', weboptions('MediaType', 'application/json', 'RequestMethod', 'post'));
                catch
                end
            end
            if ~isempty(obj.Process)
                obj.Process.destroy();
                obj.Process = [];
            end
            obj.ProxyUrl = '';
        end

        function configure(obj, filePath, workDir)
            data = struct('filePath', filePath, 'workDir', workDir);
            webwrite([obj.ProxyUrl '/config'], jsonencode(data), ...
                weboptions('MediaType', 'application/json', 'RequestMethod', 'post'));
        end

        function url = getUrl(obj)
            url = obj.ProxyUrl;
        end
    end
end
