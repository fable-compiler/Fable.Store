const path = require('path');

const mode = process.env.NODE_ENV || 'development';
const prod = mode === 'production';

module.exports = {
	entry: {
		bundle: ['./src/main.js']
	},
	resolve: {
		alias: {
			svelte: path.resolve('node_modules', 'svelte')
		},
		extensions: ['.mjs', '.js', '.svelte'],
		mainFields: ['svelte', 'browser', 'module', 'main']
	},
	output: {
		path: __dirname + '/public',
		filename: '[name].js',
		chunkFilename: '[name].[id].js',
	},
    // devServer: {
    //     public: '/',
	// 	   contentBase: __dirname + '/public',
    //     port: 8080,
    //     host: '0.0.0.0',
    //     // hot: true,
    //     // inline: true
    // },
	module: {
		rules: [
			{
				test: /\.svelte$/,
				use: {
					loader: 'svelte-loader',
					options: {
						emitCss: prod,
						hotReload: !prod,
						dev: !prod,
					}
				}
			}
		]
	},
	mode,
	devtool: prod ? false: 'source-map'
};
