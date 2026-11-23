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

            % Point the SDK at the CLI found by findCliPath (the client reads
            % COPILOT_CLI_PATH and runs ".js" entry points through node).
            cliPath = CapiProxy.findCliPath();
            if ~isempty(cliPath)
                setenv('COPILOT_CLI_PATH', cliPath);
            end
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

    methods (Static)
        function cliPath = findCliPath()
            % findCliPath  Copilot CLI entry point for the E2E tests.
            %
            %   Lookup order:
            %     1. COPILOT_CLI_PATH when set and the file exists (CI points
            %        it at nodejs/node_modules/.bin/copilot, run directly);
            %     2. nodejs/node_modules/@github/copilot/npm-loader.js
            %        (CLI 1.0.113+, run with node);
            %     3. legacy nodejs/node_modules/@github/copilot-*/index.js and
            %        nodejs/node_modules/@github/copilot/index.js.
            %   Returns '' when nothing is found.
            cliPath = getenv('COPILOT_CLI_PATH');
            if ~isempty(cliPath) && isfile(cliPath)
                return;
            end
            repoRoot = fullfile(fileparts(mfilename('fullpath')), '..', '..');
            modules = fullfile(repoRoot, 'nodejs', 'node_modules', '@github');
            cliPath = fullfile(modules, 'copilot', 'npm-loader.js');
            if isfile(cliPath)
                return;
            end
            platforms = dir(fullfile(modules, 'copilot-*'));
            for i = 1:numel(platforms)
                cliPath = fullfile(modules, platforms(i).name, 'index.js');
                if platforms(i).isdir && isfile(cliPath)
                    return;
                end
            end
            cliPath = fullfile(modules, 'copilot', 'index.js');
            if ~isfile(cliPath)
                cliPath = '';
            end
        end
    end
end
