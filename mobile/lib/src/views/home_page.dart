import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';

/// Home Page
class HomePage extends StatelessWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    var pipeline1 = Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 10,
        lastTrigger: DateTime.now(),
        enabled: true,
        parameters: {});
    var pipeline2 = Pipeline(
        id: 10,
        name: "My very long action Action",
        triggerCount: 10,
        lastTrigger: DateTime.parse("2022-01-01"),
        enabled: true,
        parameters: {});
    return AerisPage(
        body: Column(children: [
      PipelineCard(pipeline: pipeline1),
      PipelineCard(pipeline: pipeline2)
    ]));
  }
}
