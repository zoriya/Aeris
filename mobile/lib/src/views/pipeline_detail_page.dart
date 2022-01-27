import 'package:flutter/material.dart';
import 'package:flutter_switch/flutter_switch.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:mobile/src/widgets/clickable_card.dart';

/// Class to get the pipeline's name in route's arguments
class PipelineDetailPageArguments {
  final Pipeline
      pipeline; // TODO Should be later defined as an int, to fetch from db, or as the object

  PipelineDetailPageArguments(this.pipeline);
}

// Page for a Pipeline's details
class PipelineDetailPage extends StatefulWidget {
  //final String pipelineName; // TODO Define as int later on
  const PipelineDetailPage({Key? key}) : super(key: key);

  @override
  State<PipelineDetailPage> createState() => _PipelineDetailPageState();
}

class _PipelineDetailPageState extends State<PipelineDetailPage> {
  @override
  Widget build(BuildContext context) {
    final PipelineDetailPageArguments arguments = ModalRoute.of(context)!
        .settings
        .arguments as PipelineDetailPageArguments;
    Pipeline pipeline = arguments.pipeline;
    return AerisCardPage(
        body: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
      const SizedBox(height: 10),
      Row(
        children: [
          Expanded(
            flex: 7,
            child: Column(
              children: [
                Align(
                  alignment: Alignment.centerLeft,
                  child: Text(pipeline.name,
                      style: const TextStyle(
                        fontSize: 25,
                      )),
                ),
                const SizedBox(height: 10),
                Align(
                  alignment: Alignment.centerLeft,
                  child: Text(pipeline.trigger.lastToString(),
                      style: const TextStyle(
                        fontSize: 17,
                      )),
                ),
              ],
            ),
          ),
          Expanded(
              flex: 3,
              child: Column(
                children: [
                  const SizedBox(height: 10),
                  Align(
                    alignment: Alignment.center,
                    child: FlutterSwitch(
                      activeColor: Colors.green,
                      width: 60,
                      value: pipeline.enabled,
                      onToggle: (value) {
                        setState(() {
                          pipeline.enabled = !pipeline.enabled;
                        });
                      },
                    ),
                  ),
                  const SizedBox(height: 10),
                  Align(
                    alignment: Alignment.center,
                    child: Text(pipeline.enabled ? "Enabled" : "Disabed",
                        style: const TextStyle(fontSize: 13)),
                  ),
                ],
              ))
        ],
      ),
      const SizedBox(height: 40),
      const Text("Action", style: TextStyle(fontWeight: FontWeight.w500)),
      const Text("Danger Zone", style: TextStyle(fontWeight: FontWeight.w500)),
      const SizedBox(height: 5),
      ClickableCard(
          color: Theme.of(context).colorScheme.error,
          elevation: 5,
          body: Container(
              child: Text(
                "Delete a Pipeline",
                textAlign: TextAlign.center,
                style: TextStyle(
                    color: Theme.of(context).colorScheme.onSecondary,
                    fontSize: 20,
                    fontWeight: FontWeight.w600),
              ),
              width: double.infinity,
              padding: const EdgeInsets.only(top: 15, bottom: 15)),
          onTap: () {
            print("Delete pipeline"); // TODO remove pipeline
          }),
    ]));
  }
}
