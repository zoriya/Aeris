import { Button } from "@mui/material";
import DeleteIcon from "@mui/icons-material/Delete"
import "./GenericButton.css"
import Card from "@mui/material/Card"
import { Box, CardContent, Typography, CardMedia, IconButton } from "@mui/material";
import MoreVertIcon from '@mui/icons-material/MoreVert';


export default function GenericButton()
{
    return (

        <Card sx={{ display: 'flex', alignItems:"center", borderRadius:"15px", width:"500px", padding:"10px" }}>
            <Box sx={{ flex: '1 0 auto' }}>
                <CardMedia
                    component="img"
                    sx={{ width: 70 }}
                    image={ "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg" }
                    alt={ "service2.altText "}
                />
            </Box>
            <Box sx={{ flex: '3 0 auto' }}>
                <CardContent>
                    <Typography variant="h3" noWrap align="center" >
                        { "title" }
                    </Typography>
                </CardContent>
            </Box>
            <Box sx={{ flex: '1 0 auto' }}>
                <IconButton color="secondary" aria-label="Options" component="span">
                    <MoreVertIcon />
                </IconButton>
            </Box>
		</Card>
    );
}
