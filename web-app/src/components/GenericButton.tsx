import { Button } from "@mui/material";
import DeleteIcon from "@mui/icons-material/Delete"
import "./GenericButton.css"
import Card from "@mui/material/Card"
import { Box, CardContent, Typography, CardMedia, IconButton, Avatar } from "@mui/material";
import MoreVertIcon from '@mui/icons-material/MoreVert';

import type { ServiceProps } from './types'


interface GenericButtonProps {
    title: string,

    service: ServiceProps

}

export default function GenericButton({ title, service }: GenericButtonProps)
{
    return (

        <Card className="GenericButton" sx={{ alignItems:"center", borderRadius:"15px", padding:"10px" }}>
            <Box className="GenericButtonMedia"  sx={{ display:'flex', flex: '1 0 auto', alignItems:"center" }}>
                <CardMedia
                    component="img"
                    sx={{ width: 70 }}
                    image={ service.imageSrc }
                    alt={ service.altText }
                />
            </Box>
            <Box sx={{ flex: '5 0 auto' }}>
                <CardContent>
                    <Typography variant="h4" noWrap align="center" >
                        { title }
                    </Typography>
                </CardContent>
            </Box>
            <Box sx={{ flex: '1 0 auto' }}>
                <IconButton color="secondary" aria-label="Options" component="span" >
                    <MoreVertIcon />
                </IconButton>
            </Box>
		</Card>
    );
}
