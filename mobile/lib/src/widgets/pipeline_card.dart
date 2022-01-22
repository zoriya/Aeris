import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';

/// Widget for Action-reaction card on home page
class PipelineCard extends StatelessWidget {
  // Pipeline base object
  final Pipeline pipeline;

  const PipelineCard({Key? key, required this.pipeline}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Card(
        child: Container(
            width: double.infinity,
            padding: const EdgeInsets.all(25),
            child: Row(children: [
              Expanded(
                  flex: 4,
                  child: Column(
                    children: [Text(pipeline.name)],
                  )),
              Expanded(flex: 4, child: Column(children: [Text(pipeline.name)])),
              Expanded(
                  flex: 2,
                  child: Column(
                      children: const [
                        Icon(
                          Icons.arrow_forward_ios,
                          color: Colors.grey,
                        )
                      ],
                      mainAxisAlignment: MainAxisAlignment.center,
                      crossAxisAlignment: CrossAxisAlignment.center)),
            ])),
        shape: const RoundedRectangleBorder(
            borderRadius: BorderRadius.all(Radius.circular(25))));
  }
}
