import "./PipelineBox.css"

interface PipelineBoxProps
{
	// title of the pipeline box
	title: string,
	// utc in seconds
	lastExecutionTime: string
}


export default function PipelineBox({ title, lastExecutionTime }: PipelineBoxProps)
{
	return (
	<article className="PipelineBox">
		<div className="PipelineBoxImage">logo</div>
		<header className="PipelineBoxHeader" >{title}</header>
		<footer className="PipelineBoxFooter" >Triggered {lastExecutionTime} ago</footer>
	</article>
	);
}