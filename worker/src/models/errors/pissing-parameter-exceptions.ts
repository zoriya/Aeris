class MissingParameterException extends AerisError {
	constructor(missingParameters: string) {
		var formatted = `Expected parameter '${missingParameters}', none found.`
		super(formatted);
	}
}