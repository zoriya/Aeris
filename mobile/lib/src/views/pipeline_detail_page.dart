import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';

/// Class to get the pipeline's name in route's arguments
class PipelineDetailPageArguments {
  final String
      pipelineName; // TODO Should be later defined as an int, to fetch from db, or as the object

  PipelineDetailPageArguments(this.pipelineName);
}

// Page for a Pipeline's details
class PipelineDetailPage extends StatelessWidget {
  //final String pipelineName; // TODO Define as int later on
  const PipelineDetailPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final PipelineDetailPageArguments arguments = ModalRoute.of(context)!
        .settings
        .arguments as PipelineDetailPageArguments;
    return AerisCardPage(
      body: Text(arguments.pipelineName, textAlign: TextAlign.center),
    );
  }
}
