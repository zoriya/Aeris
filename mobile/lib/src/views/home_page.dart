import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';

/// Home Page
class HomePage extends StatelessWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return AerisPage(body: PipelineCard(pipeline: Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 10,
        lastTrigger: DateTime.now(),
        enabled: true,
        parameters: {})));
  }
}
