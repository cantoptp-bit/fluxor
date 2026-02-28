/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as GuildActionCreators from '@app/actions/GuildActionCreators';
import * as InviteActionCreators from '@app/actions/InviteActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as NavigationActionCreators from '@app/actions/NavigationActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import ChannelStore from '@app/stores/ChannelStore';
import {ExternalLink} from '@app/components/common/ExternalLink';
import {Form} from '@app/components/form/Form';
import {Input, Textarea} from '@app/components/form/Input';
import styles from '@app/components/modals/AddGuildModal.module.css';
import {AssetCropModal, AssetType} from '@app/components/modals/AssetCropModal';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {Checkbox} from '@app/components/uikit/checkbox/Checkbox';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import {Routes} from '@app/Routes';
import GuildListStore from '@app/stores/GuildListStore';
import GuildStore from '@app/stores/GuildStore';
import PermissionStore from '@app/stores/PermissionStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import {isAnimatedFile} from '@app/utils/AnimatedImageUtils';
import * as AvatarUtils from '@app/utils/AvatarUtils';
import {openFilePicker} from '@app/utils/FilePickerUtils';
import {getInitialsLength} from '@app/utils/GuildInitialsUtils';
import * as InviteUtils from '@app/utils/InviteUtils';
import * as StringUtils from '@app/utils/StringUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {HouseIcon, LinkIcon, UsersThreeIcon} from '@phosphor-icons/react';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	DISCOVERY_DESCRIPTION_MAX_LENGTH,
	DISCOVERY_DESCRIPTION_MIN_LENGTH,
	DiscoveryCategories,
	DiscoveryCategoryLabels,
} from '@fluxer/constants/src/DiscoveryConstants';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useContext, useEffect, useId, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

interface GuildCreateFormInputs {
	icon?: string | null;
	name: string;
	list_in_discovery?: boolean;
	discovery_description?: string;
	discovery_category?: number;
}

interface GuildJoinFormInputs {
	code: string;
}

interface ModalFooterContextValue {
	setFooterContent: (content: React.ReactNode) => void;
}

export type AddGuildModalView = 'landing' | 'create_server' | 'create_guild' | 'join_guild';

const ModalFooterContext = React.createContext<ModalFooterContextValue | null>(null);

const ActionButton = ({onClick, icon, label}: {onClick: () => void; icon: React.ReactNode; label: string}) => (
	<button type="button" onClick={onClick} className={styles.actionButton}>
		<span className={styles.actionIcon}>{icon}</span>
		<span className={styles.actionLabel}>{label}</span>
	</button>
);

export const AddGuildModal = observer(({initialView = 'landing'}: {initialView?: AddGuildModalView} = {}) => {
	const {t} = useLingui();
	const [view, setView] = useState<AddGuildModalView>(initialView);
	const [footerContent, setFooterContent] = useState<React.ReactNode>(null);

	const getTitle = (): string => {
		switch (view) {
			case 'landing':
				return t`Add a Server or Community`;
			case 'create_server':
				return t`Create a Server`;
			case 'create_guild':
				return t`Create a Community`;
			case 'join_guild':
				return t`Join a Server or Community`;
			default:
				return t`Add a Server or Community`;
		}
	};

	const contextValue = useMemo(
		() => ({
			setFooterContent,
		}),
		[],
	);

	return (
		<ModalFooterContext.Provider value={contextValue}>
			<Modal.Root size="small" centered>
				<Modal.Header title={getTitle()} />

				<Modal.Content contentClassName={styles.content}>
					{view === 'landing' && <LandingView onViewChange={setView} />}
					{view === 'create_server' && <GuildCreateForm guildType="server" />}
					{view === 'create_guild' && <GuildCreateForm guildType="community" />}
					{view === 'join_guild' && <GuildJoinForm />}
				</Modal.Content>

				{footerContent && <Modal.Footer>{footerContent}</Modal.Footer>}
			</Modal.Root>
		</ModalFooterContext.Provider>
	);
});

const LandingView = observer(({onViewChange}: {onViewChange: (view: AddGuildModalView) => void}) => {
	const {t} = useLingui();

	return (
		<div className={styles.landingContainer}>
			<p>
				<Trans>Create a new server or community, or join an existing one.</Trans>
			</p>
			<p className={styles.landingHint}>
				<Trans>
					Servers are for friends and groups. Communities are for creators, YouTubers, and public figures to
					connect with their audience.
				</Trans>
			</p>

			<div className={styles.actionButtons}>
				<ActionButton
					onClick={() => onViewChange('create_server')}
					icon={<HouseIcon size={24} />}
					label={t`Create Server`}
				/>
				<ActionButton
					onClick={() => onViewChange('create_guild')}
					icon={<UsersThreeIcon size={24} />}
					label={t`Create Community`}
				/>
				<ActionButton
					onClick={() => onViewChange('join_guild')}
					icon={<LinkIcon size={24} weight="bold" />}
					label={t`Join with invite`}
				/>
			</div>
		</div>
	);
});

type GuildType = 'server' | 'community';

const GuildCreateForm = observer(({guildType}: {guildType: GuildType}) => {
	const {t} = useLingui();
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const isServer = guildType === 'server';
	const form = useForm<GuildCreateFormInputs>({
		defaultValues: {
			name: '',
			list_in_discovery: false,
			discovery_description: '',
			discovery_category: DiscoveryCategories.OTHER,
		},
	});
	const modalFooterContext = useContext(ModalFooterContext);
	const formId = useId();
	const guildNamePlaceholders = useMemo(
		() => [
			t`The Midnight Gamers`,
			t`Study Buddies United`,
			t`Creative Minds Collective`,
			t`Bookworms Anonymous`,
			t`Artists' Corner`,
			t`Dev Den`,
			t`Band Practice Room`,
			t`Volunteer Heroes`,
			t`Hobby Haven`,
			t`Class of '24`,
			t`Team Alpha`,
			t`Family Reunion`,
			t`Project X`,
			t`Weekend Warriors`,
			t`Movie Night Crew`,
			t`Neighborhood Watch`,
			t`Professional Peers`,
			t`Support Circle`,
			t`Coffee Chat`,
			t`Game Night`,
			t`Study Hall`,
			t`Creative Writing Club`,
			t`Photography Club`,
			t`Music Lovers`,
			t`Fitness Friends`,
			t`Foodie Friends`,
			t`Travel Buddies`,
			t`Movie Club`,
			t`Board Game Night`,
			t`Coding Crew`,
			t`Art Club`,
			t`Book Club`,
			t`Sports Fans`,
			t`Gaming Community`,
			t`Study Group`,
			t`Work Friends`,
			t`Family Chat`,
			t`Friends Forever`,
			t`The Squad`,
			t`Our Hangout`,
		],
		[],
	);

	const randomPlaceholder = useMemo(() => {
		const randomIndex = Math.floor(Math.random() * guildNamePlaceholders.length);
		return guildNamePlaceholders[randomIndex];
	}, [guildNamePlaceholders]);

	const nameValue = form.watch('name');

	const initials = useMemo(() => {
		const raw = (nameValue || '').trim();
		if (!raw) return '';
		return StringUtils.getInitialsFromName(raw);
	}, [nameValue]);

	const initialsLength = useMemo(() => (initials ? getInitialsLength(initials) : null), [initials]);

	const handleIconUpload = useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: 'image/*'});
			if (!file) return;

			if (file.size > 10 * 1024 * 1024) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Icon file is too large. Please choose a file smaller than 10MB.`,
				});
				return;
			}

			const animated = await isAnimatedFile(file);

			if (animated) {
				ToastActionCreators.createToast({
					type: 'error',
					children: isServer
						? t`Animated icons are not supported when creating a new server. Please use JPEG, PNG, or WebP.`
						: t`Animated icons are not supported when creating a new community. Please use JPEG, PNG, or WebP.`,
				});
				return;
			}

			const base64 = await AvatarUtils.fileToBase64(file);

			ModalActionCreators.push(
				modal(() => (
					<AssetCropModal
						assetType={AssetType.GUILD_ICON}
						imageUrl={base64}
						sourceMimeType={file.type}
						onCropComplete={(croppedBlob) => {
							const reader = new FileReader();
							reader.onload = () => {
								const croppedBase64 = reader.result as string;
								form.setValue('icon', croppedBase64);
								setPreviewIconUrl(croppedBase64);
								form.clearErrors('icon');
							};
							reader.onerror = () => {
								ToastActionCreators.createToast({
									type: 'error',
									children: t`Failed to process the cropped image. Please try again.`,
								});
							};
							reader.readAsDataURL(croppedBlob);
						}}
						onSkip={() => {
							form.setValue('icon', base64);
							setPreviewIconUrl(base64);
							form.clearErrors('icon');
						}}
					/>
				)),
			);
		} catch {
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>That image is invalid. Please try another one.</Trans>,
			});
		}
	}, [form, isServer]);

	const onSubmit = useCallback(
		async (data: GuildCreateFormInputs) => {
			const payload: Parameters<typeof GuildActionCreators.create>[0] = {
				icon: data.icon,
				name: data.name,
				guild_type: guildType,
			};
			if (!isServer && data.list_in_discovery && data.discovery_description != null && data.discovery_category != null) {
				payload.list_in_discovery = true;
				payload.discovery_description = data.discovery_description;
				payload.discovery_category = data.discovery_category;
			}
			const guild = await GuildActionCreators.create(payload);
		// Add guild and system channel to stores so navigation shows the community
		// (GUILD_CREATE is only sent in FLUXER_LITE_MODE, so we seed the client state here)
		GuildStore.addGuildFromApiResponse(guild);
		if (guild.system_channel_id) {
			ChannelStore.handleChannelCreate({
				channel: {
					id: guild.system_channel_id,
					guild_id: guild.id,
					type: ChannelTypes.GUILD_TEXT,
					name: 'general',
					topic: null,
					position: 0,
				},
			});
		}
		GuildListStore.handleGuild(guild);
		PermissionStore.handleGuild();
		ModalActionCreators.pop();
		NavigationActionCreators.selectChannel(guild.id, guild.system_channel_id || undefined);
		},
		[guildType],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	const listInDiscovery = form.watch('list_in_discovery');
	const discoveryDescription = form.watch('discovery_description');
	const discoveryInvalid =
		!isServer &&
		(listInDiscovery === true) &&
		((discoveryDescription?.trim().length ?? 0) < DISCOVERY_DESCRIPTION_MIN_LENGTH ||
			(discoveryDescription?.length ?? 0) > DISCOVERY_DESCRIPTION_MAX_LENGTH);

	useEffect(() => {
		const isNameEmpty = !nameValue?.trim();

		modalFooterContext?.setFooterContent(
			<>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleSubmit}
					submitting={isSubmitting}
					disabled={isNameEmpty || discoveryInvalid}
				>
					{isServer ? <Trans>Create Server</Trans> : <Trans>Create Community</Trans>}
				</Button>
			</>,
		);

		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, nameValue, discoveryInvalid]);

	const handleClearIcon = useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
	}, [form]);

	return (
		<div className={styles.formContainer}>
			<p>
				{isServer ? (
					<Trans>Create a server for you and your friends to chat.</Trans>
				) : (
					<Trans>
						Communities are for creators, YouTubers, and public figures to connect with their audience.
					</Trans>
				)}
			</p>

			<Form
				form={form}
				onSubmit={handleSubmit}
				id={formId}
				aria-label={isServer ? t`Create server form` : t`Create community form`}
			>
				<div className={styles.iconSection}>
					<div className={styles.iconSectionInner}>
						<div className={styles.iconLabel}>
							{isServer ? <Trans>Server Icon</Trans> : <Trans>Community Icon</Trans>}
						</div>
						<div className={styles.iconPreview}>
							{previewIconUrl ? (
								<div className={styles.iconImage} style={{backgroundImage: `url(${previewIconUrl})`}} />
							) : (
								<div className={styles.iconPlaceholder} data-initials-length={initialsLength}>
									{initials ? <span className={styles.iconInitials}>{initials}</span> : null}
								</div>
							)}
							<div className={styles.iconActions}>
								<div className={styles.iconButtons}>
									<Button variant="secondary" small={true} onClick={handleIconUpload}>
										{previewIconUrl ? <Trans>Change Icon</Trans> : <Trans>Upload Icon</Trans>}
									</Button>
									{previewIconUrl && (
										<Button variant="secondary" small={true} onClick={handleClearIcon}>
											<Trans>Remove Icon</Trans>
										</Button>
									)}
								</div>
								<div className={styles.iconHint}>
									<Trans>JPEG, PNG, WebP. Max 10MB. Recommended: 512×512px</Trans>
								</div>
							</div>
						</div>
						{form.formState.errors.icon?.message && (
							<p className={styles.iconError}>{form.formState.errors.icon.message}</p>
						)}
					</div>

					<Input
						{...form.register('name')}
						autoFocus={true}
						error={form.formState.errors.name?.message}
						label={isServer ? t`Server Name` : t`Community Name`}
						minLength={1}
						maxLength={100}
						name="name"
						placeholder={randomPlaceholder}
						required={true}
						type="text"
					/>
					<p className={styles.guidelines}>
						{isServer ? (
							<Trans>
								By creating a server, you agree to follow and uphold the{' '}
								<ExternalLink href={Routes.guidelines()} className={styles.guidelinesLink}>
									Fluxer Community Guidelines
								</ExternalLink>
								.
							</Trans>
						) : (
							<Trans>
								By creating a community, you agree to follow and uphold the{' '}
								<ExternalLink href={Routes.guidelines()} className={styles.guidelinesLink}>
									Fluxer Community Guidelines
								</ExternalLink>
								.
							</Trans>
						)}
					</p>
					{!isServer && (
						<div className={styles.discoverySection}>
							<Checkbox
								checked={form.watch('list_in_discovery') ?? false}
								onChange={(checked) => form.setValue('list_in_discovery', checked)}
							>
								<Trans>List this community in the Community Explorer</Trans>
							</Checkbox>
							{(form.watch('list_in_discovery') ?? false) && (
								<>
									<Textarea
										{...form.register('discovery_description', {
											minLength: {
												value: DISCOVERY_DESCRIPTION_MIN_LENGTH,
												message: t`Description must be at least ${DISCOVERY_DESCRIPTION_MIN_LENGTH} characters`,
											},
											maxLength: {
												value: DISCOVERY_DESCRIPTION_MAX_LENGTH,
												message: t`Description must be at most ${DISCOVERY_DESCRIPTION_MAX_LENGTH} characters`,
											},
										})}
										error={form.formState.errors.discovery_description?.message}
										label={t`Short description for discovery`}
										maxLength={DISCOVERY_DESCRIPTION_MAX_LENGTH}
										placeholder={t`What is your community about?`}
										rows={3}
									/>
									<div className={styles.selectWrapper}>
										<label className={styles.selectLabel} htmlFor={`${formId}-discovery-category`}>
											{t`Category`}
										</label>
										<select
											id={`${formId}-discovery-category`}
											{...form.register('discovery_category', {valueAsNumber: true})}
											className={styles.select}
										>
											{(Object.entries(DiscoveryCategoryLabels) as Array<[string, string]>).map(
												([value, label]) => (
													<option key={value} value={value}>
														{label}
													</option>
												),
											)}
										</select>
									</div>
								</>
							)}
						</div>
					)}
				</div>
			</Form>
		</div>
	);
});

const GuildJoinForm = observer(() => {
	const {t, i18n} = useLingui();
	const form = useForm<GuildJoinFormInputs>({defaultValues: {code: ''}});
	const modalFooterContext = useContext(ModalFooterContext);
	const formId = useId();
	const randomInviteCode = useMemo(() => {
		const chars = 'abcdefghijklmnopqrstuvwxyz0123456789';
		const length = Math.floor(Math.random() * 7) + 6;
		let result = '';
		for (let i = 0; i < length; i++) {
			result += chars.charAt(Math.floor(Math.random() * chars.length));
		}
		return result;
	}, []);

	const onSubmit = useCallback(
		async (data: GuildJoinFormInputs) => {
			const parsedCode = InviteUtils.findInvite(data.code) ?? data.code;
			const invite = await InviteActionCreators.fetch(parsedCode);
			await InviteActionCreators.acceptAndTransitionToChannel(invite.code, i18n);
			ModalActionCreators.pop();
		},
		[i18n],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});

	const codeValue = form.watch('code');

	useEffect(() => {
		const isCodeEmpty = !codeValue?.trim();

		modalFooterContext?.setFooterContent(
			<>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSubmit} submitting={isSubmitting} disabled={isCodeEmpty}>
					<Trans>Join</Trans>
				</Button>
			</>,
		);

		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, codeValue]);

	return (
		<div className={styles.formContainer}>
			<p>
				<Trans>Enter the invite link to join a server or community.</Trans>
			</p>

			<Form form={form} onSubmit={handleSubmit} id={formId} aria-label={t`Join server or community form`}>
				<div className={styles.iconSection}>
					<Input
						{...form.register('code')}
						autoFocus={true}
						error={form.formState.errors.code?.message}
						label={t`Invite Link`}
						minLength={1}
						maxLength={100}
						name="code"
						placeholder={`${RuntimeConfigStore.inviteEndpoint}/${randomInviteCode}`}
						required={true}
						type="text"
					/>
				</div>
			</Form>
		</div>
	);
});
