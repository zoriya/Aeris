import "./PipelineBox.css"

interface PipelineBoxProps
{
	num: number
}


export default function PipelineBox({ num }: PipelineBoxProps)
{
	return (
	<div className="PipelineBox">
		slt {num}
	</div>
	);
}