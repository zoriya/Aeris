
/// Check if the reaction parameters contains the required keys
function checkParams(params: any, required: Array<string>) {
	required.forEach((param, _, __) => {
		if (!(param in params))
			throw new MissingParameterException(param);
	});
}