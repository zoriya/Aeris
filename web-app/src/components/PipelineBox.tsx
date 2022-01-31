import Card from '@mui/material/Card';
import CardContent from '@mui/material/CardContent';
import CardMedia from '@mui/material/CardMedia';
import Typography from '@mui/material/Typography';
import { CardActionArea } from '@mui/material';

import { useTheme } from '@mui/material/styles';
import Box from '@mui/material/Box';
import IconButton from '@mui/material/IconButton';
import SkipPreviousIcon from '@mui/icons-material/SkipPrevious';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import SkipNextIcon from '@mui/icons-material/SkipNext';

import ArrowForwardIcon from '@mui/icons-material/ArrowForward';

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
	const theme = useTheme();

	return (
	<div>
		<Card sx={{ display: 'flex' }}>
			<Box sx={{ display: 'flex', flexDirection: 'column' }}>
				<CardContent sx={{ flex: '1 0 auto' }}>
					<Typography component="div" variant="h5">
						Live From Space
					</Typography>
					<Typography variant="subtitle1" color="text.secondary" component="div">
						Mac Milleruhuy
					</Typography>
				</CardContent>
				<Box sx={{ display: 'flex', alignItems: 'center', pl: 1, pb: 1 }}>
					<IconButton aria-label="previous">
						{theme.direction === 'rtl' ? <SkipNextIcon /> : <SkipPreviousIcon />}
					</IconButton>
					<IconButton aria-label="play/pause">
						<PlayArrowIcon sx={{ height: 38, width: 38 }} />
					</IconButton>
					<IconButton aria-label="next">
						{theme.direction === 'rtl' ? <SkipPreviousIcon /> : <SkipNextIcon />}
					</IconButton>
				</Box>
				<CardMedia
				component="img"
				sx={{ width: 151 }}
				image="https://mui.com/static/images/cards/live-from-space.jpg"
				alt="Live from space album cover"
			/>
			</Box>
    </Card>

	<br />
		<Card sx={{ width: 345, height: 345 }}>
      <CardActionArea>
        <CardMedia
          component="img"
          height="140"
          image="https://mui.com/static/images/cards/contemplative-reptile.jpg"
          alt="green iguana"
        />
        <CardContent>
          <Typography gutterBottom variant="h5" component="div">
            Lizard
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Lizards are a widespread group of squamate reptiles, with over 6,000
            species, ranging across all continents except Antarctica
          </Typography>
        </CardContent>
      </CardActionArea>
    </Card>
		<br />
	<Card sx={{ }}>
		<CardActionArea>
		<Box sx={{ flexDirection: 'row' }}>
		<img
		  width={100}
		  height={100}
          src="https://upload.wikimedia.org/wikipedia/commons/7/72/YouTube_social_white_square_%282017%29.svg"
          alt="green iguana"
        />
		<ArrowForwardIcon />
		<img
		  width={100}
		  height={100}
          src="https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg"
          alt="green iguana"
        />
			</Box>
			<CardContent>
				<Typography gutterBottom variant="h5" component="div">
					Update like playlist
				</Typography>
				<Typography variant="body2" color="text.secondary">
					Triggered 10min ago.
				</Typography>
			</CardContent>
		</CardActionArea>
	</Card>
		<article className="PipelineBox">
			<div className="PipelineBoxImage">logo</div>
			<header className="PipelineBoxHeader" >{title}</header>
			<footer className="PipelineBoxFooter" >Triggered {lastExecutionTime} ago</footer>
		</article>
	</div>
	);
}