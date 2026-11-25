# AI content provenance

Learn what the Copilot SDK does and does not do to mark AI-generated content, and how your application can meet its own content provenance requirements.

Content provenance is the record of where a piece of content came from and whether it has changed since. Common provenance signals include C2PA metadata, perceptible (visible) watermarks, and imperceptible watermarks embedded by the model provider. For background, see [Content provenance and safety](https://docs.github.com/copilot/concepts/agent-capabilities/agent-powered-workflows-and-operations/image-generation#content-provenance-and-safety) in the GitHub Docs.

## What the SDK provides

The SDK passes model output to your application as session events. It applies one optional marking of its own, a visible label on generated images, and otherwise leaves provenance signals to the model provider and to your application.

| Content | What the SDK does |
|---|---|
| Text responses | No watermarking. There is no SDK, user, or enterprise setting to watermark text. |
| Generated and edited images | Optional visible "Made with AI" label when you opt in with `imageGeneration.watermark`. |
| Files an agent writes, such as PDF, DOCX, XLSX, or ZIP | No C2PA manifests or AI-generated metadata are added. |

## Text output

The SDK does not watermark text, and you cannot turn text watermarking on through the SDK. Whether a text response carries a watermark depends entirely on the model provider that serves the request. The SDK does not add, remove, or detect text watermarks.

If your application must use text from providers that watermark it, control which models and providers sessions can use. See [Meeting provenance requirements](#meeting-provenance-requirements).

## Generated images

Image generation is disabled by default for SDK sessions. When you opt in with `imageGeneration.enabled`, you can also set `imageGeneration.watermark` to request a visible "Made with AI" label on images the agent generates or edits.

> [!WARNING]
> Image generation and the `watermark` option are experimental. The option names and behavior can change in a future release.

The `watermark` option behaves as follows:

* It overrides the user's `imageGenerationWatermark` setting for this session. That setting defaults to `false`.
* It does not turn on image generation. Set `enabled` as well.
* It is not persisted. Supply it again when you resume a session after a runtime restart.
* It does not enforce an organization or enterprise policy. Your application decides whether to request the label.

Image generation is not supported with BYOK providers. Any C2PA metadata on generated images comes from the image model provider, not from the SDK. To preserve provenance, do not strip or alter that metadata or the visible label when you store or deliver images.

<details open>
<summary><strong>TypeScript</strong></summary>

```typescript
const session = await client.createSession({
    tools: {
        imageGeneration: {
            enabled: true,
            watermark: true,
        },
    },
});
```

</details>
<details>
<summary><strong>Python</strong></summary>

<!-- docs-validate: skip -->

```python
session = await client.create_session(
    tools={"image_generation": {"enabled": True, "watermark": True}},
)
```

</details>
<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: skip -->

```go
session, err := client.CreateSession(ctx, &copilot.CreateSessionOptions{
	Tools: copilot.SessionToolOptions{
		ImageGeneration: &copilot.ImageGenerationConfig{
			Enabled:   copilot.Bool(true),
			Watermark: copilot.Bool(true),
		},
	},
})
```

</details>
<details>
<summary><strong>.NET</strong></summary>

<!-- docs-validate: skip -->

```csharp
#pragma warning disable MCPSDK001 // Image generation is experimental.

var session = await client.CreateSessionAsync(new CreateSessionOptions
{
    Tools = new SessionToolOptions
    {
        ImageGeneration = new ImageGenerationConfig
        {
            Enabled = true,
            Watermark = true
        }
    }
});
```

</details>
<details>
<summary><strong>Java</strong></summary>

<!-- docs-validate: skip -->

```java
CopilotSession session = client
        .createSession(new CreateSessionOptions()
                .setTools(new SessionToolOptions()
                        .setImageGeneration(new ImageGenerationConfig()
                                .setEnabled(true)
                                .setWatermark(true))))
        .get();
```

</details>
<details>
<summary><strong>Rust</strong></summary>

<!-- docs-validate: skip -->

```rust
let session = client
    .create_session(CreateSessionOptions::default().with_tools(|tools| {
        tools.with_image_generation(
            ImageGenerationConfig::new()
                .with_enabled(true)
                .with_watermark(true),
        )
    }))
    .await?;
```

</details>

## Files written by agents

When an agent creates or edits files, such as PDF, DOCX, XLSX, or ZIP archives, the SDK does not add C2PA manifests, AI-generated metadata, or any other provenance marking to them. This also applies to text the agent places inside those files.

If your application delivers AI-generated content as files and must mark them, apply provenance yourself before delivery. For example, sign files with a C2PA manifest or set document metadata using a library for the file format.

## Meeting provenance requirements

If your application has content provenance requirements, combine the controls that apply to your scenario:

* **Choose models and providers**: use models from providers that watermark their output, and select them explicitly with the session `model` option.
* **Restrict available models**: organization and enterprise owners can limit which Copilot models are available through model policies. See [Managing availability of models in your enterprise](https://docs.github.com/copilot/how-tos/administer-copilot/manage-for-enterprise/manage-availability-of-default-models).
* **Bring your own key**: route requests to a provider endpoint you control, where you can turn on any watermarking that the provider offers. See [BYOK](../auth/byok.md).
* **Request the image label**: set `imageGeneration.watermark` to `true` on every session that enables image generation.
* **Stamp delivered files**: add C2PA manifests or metadata to files before your application delivers them.

## Further reading

* [About image generation in GitHub Copilot](https://docs.github.com/copilot/concepts/agent-capabilities/agent-powered-workflows-and-operations/image-generation): image generation, content provenance, and safety
* [BYOK](../auth/byok.md): use your own model provider keys and endpoints
* [Image input](./image-input.md): send images to sessions as attachments
